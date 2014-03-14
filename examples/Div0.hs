{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Divide by 0 example in a simple arithmetic language.

module Div0 where

import Test.QuickCheck
import Test.SmartCheck
import Control.Monad

import GHC.Generics
import Data.Typeable

-----------------------------------------------------------------

data Exp = C Int
         | Add Exp Exp
         | Div Exp Exp
  deriving (Show, Typeable, Generic)

instance SubTypes Exp

eval :: Exp -> Maybe Int
eval (C i) = Just i
eval (Add e0 e1) =
  liftM2 (+) (eval e0) (eval e1)
eval (Div e0 e1) =
  let e = eval e1 in
  if e == Just 0 then Nothing
    else liftM2 div (eval e0) e

instance Arbitrary Exp where
  arbitrary = sized mkM
    where
    mkM 0 = liftM C arbitrary
    mkM n = oneof [ liftM2 Add mkM' mkM'
                  , liftM2 Div mkM' mkM' ]
      where mkM' = mkM =<< choose (0,n-1)

  -- shrink (C i)       = map C (shrink i)
  -- shrink (Add e0 e1) = [e0, e1]
  -- shrink (Div e0 e1) = [e0, e1]

-- property: so long as 0 isn't in the divisor, we won't try to divide by 0.
-- It's false: something might evaluate to 0 still.
prop_div :: Exp -> Property
prop_div e = divSubTerms e ==> eval e /= Nothing
-- prop_div e = property $ case x of
--                           Nothing -> True
--                           Just True -> True
--                           _       -> False
--   where x = fmap (< 1) (eval e)

  -- precondition: no dividand in a subterm can be 0.
divSubTerms :: Exp -> Bool
divSubTerms (C _)         = True
divSubTerms (Div _ (C 0)) = False
divSubTerms (Add e0 e1)   = divSubTerms e0 && divSubTerms e1
divSubTerms (Div e0 e1)   = divSubTerms e0 && divSubTerms e1

-- div0 (A _ _) = property False
-- div0 _       = property True

-- prop_test m = case eval m of
--                 Nothing -> True
--                 Just i -> i < 5

divTest :: IO ()
divTest = smartCheck args prop_div
  where
  args = scStdArgs { qcArgs  = stdArgs
                                -- { maxSuccess = 1000
                                -- , maxSize    = 20  }
                   , format  = PrintString
                   , runForall  = True
                   }

-- Get the minimal offending sub-value.
findVal :: Exp -> (Exp,Exp)
findVal (Div e0 e1)
  | eval e1 == Just 0     = (e0,e1)
  | eval e1 == Nothing    = findVal e1
  | otherwise             = findVal e0
findVal a@(Add e0 e1)
  | eval e0 == Nothing    = findVal e0
  | eval e1 == Nothing    = findVal e1
  | eval a == Just 0      = (a,a)
findVal _                 = error "not possible"

divSubValue :: Exp
divSubValue =
  Add (Div (C 5) (C (-12))) (Add (Add (C 2) (C 4)) (Add (C 7) (Div (C 3) (Add (C (-5)) (C 5)))))

--------------------------------------------------------------------------------

-- data Foo = Foo Int Int
--          | Bar Int Int
--   deriving (Show, Typeable, Generic)

-- instance SubTypes Foo

-- instance Arbitrary Foo where
--   arbitrary = do a <- arbitrary
--                  b <- arbitrary
--                  v <- arbitrary
--                  return $ if v then Foo a b else Bar a b

-- prop_foo :: Foo -> Int -> Int -> Bool
-- prop_foo (Foo i j) k l = i /= k || j /= l || (k < 10 && l < 10)
-- prop_foo (Bar i j) k l = i /= k || j /= l || (k < 10 && l < 10)

-- runfoo :: IO ()
-- runfoo = smartCheck scStdArgs prop_foo
