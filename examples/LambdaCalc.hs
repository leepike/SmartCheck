{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- Copied from <http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html>

module LambdaCalc where

import Data.List
import Data.Tree
import Data.Typeable

import Control.Monad
import GHC.Generics

import Test.QuickCheck

import Test.SmartCheck

type Sym = String

data Expr
        = Var Sym
        | App Expr Expr
        | Lam Sym Expr
        deriving (Eq, Read, Show, Typeable, Generic)

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i e) = freeVars e \\ [i]

subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
        sub (Lam i e) =
            if v == i then
                Lam i e
            else if i `elem` fvx then
                let i' = cloneSym e i
                    e' = substVar i i' e
                in  Lam i' (sub e')
            else
                Lam i (sub e)
        fvx = freeVars x
        cloneSym e i = loop i
           where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                 vars = fvx ++ freeVars e

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v)   (Var v')    = v == v'
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s e) (Lam s' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) [] = Lam s (nf e)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl App f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

[z,s,m,n] = map (Var . (:[])) "zsmn"
app2 f x y = App (App f x) y
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ App s z
two   = Lam "s" $ Lam "z" $ App s $ App s z
three = Lam "s" $ Lam "z" $ App s $ App s $ App s z
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)

test0 = betaEq (app2 plus one two) three

---------------------------------------------------------------------------------

instance SubTypes Expr
instance SubTypes Pr

---------------------------------------------------------------------------------

data Pr = Pr Expr Expr
  deriving (Read, Show, Typeable, Generic)

instance Arbitrary Expr where
  arbitrary = sized mkE
    where
    mkE 0 = liftM Var vars
    mkE n = oneof [ liftM2 App (liftM2 Lam vars mkE') mkE'
                  , liftM2 Lam vars mkE'
                  ]
      where
      mkE' = mkE =<< choose (0, n-1)

vars = oneof $ map return ["x", "y", "z"]

instance Arbitrary Pr where
  arbitrary = do expr  <- arbitrary
                 return $ Pr expr expr

---------------------------------------------------------------------------------

-- prop0 :: Pr -> Property
-- prop0 (Pr (e0, e1)) = alphaEq e0 e1 ==> betaEq e0 e1

-- if you do a beta reduction to nf 
prop1 :: Pr -> Property
prop1 (Pr e0 e1) = -- Timeout due to possible non-termination
  within 1000 $ alphaEq e0 e1 ==> betaEq e0 (substVar "x" "y" e1)

lambdaTest :: IO ()
lambdaTest = smartCheck args prop1
  where args = scStdArgs { qcArgs = stdArgs { maxSuccess = 100
                                            , maxDiscard = 100
                                            , maxSize    = 100
                                            }
                         }

---------------------------------------------------------------------------------
-- Cruft

nonDet = App x x
  where
  x = Lam "x" (App (Var "x") (Var "x"))


xx = (App (Lam "`" (App (Lam "\SI" (Var "f")) 
                        (App (Lam "" (Var "O\172")) 
                             (Var "3UC")))))

aa (Pr a b) = alphaEq a b
