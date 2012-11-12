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
  deriving (Read, Show, Typeable, Generic)

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

  shrink (C i)       = map C (shrink i)
  shrink (Add e0 e1) = [e0, e1]
  shrink (Div e0 e1) = [e0, e1]

-- property: so long as 0 isn't in the divisor, we won't try to divide by 0.
-- It's false: something might evaluate to 0 still.
div_prop :: Exp -> Property
--div_prop e = divSubTerms e ==> eval e /= Nothing
div_prop e = property $ case x of
                          Nothing -> True
                          Just True -> True
                          _       -> False
  where x = fmap (< 1) (eval e)

  -- precondition: no dividand in a subterm can be 0.
divSubTerms :: Exp -> Bool
divSubTerms (C _)         = True
divSubTerms (Div _ (C 0)) = False
divSubTerms (Add e0 e1)   = divSubTerms e0 && divSubTerms e1
divSubTerms (Div e0 e1)   = divSubTerms e0 && divSubTerms e1

-- div0 (A _ _) = property False
-- div0 _       = property True

-- test_prop m = case eval m of
--                 Nothing -> True
--                 Just i -> i < 5
              

divTest :: IO ()
divTest = smartCheck args div_prop
  where 
  args = scStdArgs { qcArgs  = stdArgs 
                                -- { maxSuccess = 1000
                                -- , maxSize    = 20  }
                   , format  = PrintString
                   }

---------------------------------------------------------------------------------
