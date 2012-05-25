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

data M = C Int
       | A M M
       | D M M
  deriving (Read, Show, Typeable, Generic)

instance SubTypes M 

eval :: M -> Maybe Int
eval (C i) = Just i
eval (A a b) = do
  i <- eval a 
  j <- eval b
  return $ i + j
eval (D a b) = 
  if eval b == Just 0 then Nothing 
    else do i <- eval a 
            j <- eval b
            return $ i `div` j

instance Arbitrary M where
  arbitrary = sized mkM
    where
    mkM 0 = liftM C arbitrary
    mkM n = oneof [ liftM2 A mkM' mkM' 
                  , liftM2 D mkM' mkM' ]
      where mkM' = mkM =<< choose (0,n-1)

  -- shrink (C _)   = []
  -- shrink (A a b) = [a, b]
  -- shrink (D a b) = [a, b]

-- property: so long as 0 isn't in the divisor, we won't try to divide by 0.
-- It's false: something might evaluate to 0 still.
div_prop :: M -> Property
div_prop m = divSubTerms m ==> eval m /= Nothing

  -- precondition: no dividand in a subterm can be 0.
divSubTerms :: M -> Bool
divSubTerms (C _)       = True
divSubTerms (D _ (C 0)) = False
divSubTerms (A m0 m1)   = divSubTerms m0 && divSubTerms m1
divSubTerms (D m0 m1)   = divSubTerms m0 && divSubTerms m1

-- div0 (A _ _) = property False
-- div0 _       = property True

divTest :: IO ()
divTest = smartCheck args div_prop
  where 
  args = scStdArgs { qcArgs = stdArgs 
                                -- { maxSuccess = 1000
                                -- , maxSize    = 20  }
                   , treeShow = PrntString
                   }

---------------------------------------------------------------------------------
