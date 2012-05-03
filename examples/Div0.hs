{-# LANGUAGE DeriveDataTypeable #-}

-- | Divide by 0 example in a simple arithmetic language.

module Div0 where

import Test.QuickCheck
import Test.SmartCheck
import Control.Monad
import Data.Data
import Data.Tree

data M = C Int
       | A M M
       | D M M
  deriving (Read, Show, Data, Typeable, Eq)

mkTypes :: M -> M -> Forest SubT
mkTypes m0 m1 = [ Node (subT m0) (subTypes m0)
                , Node (subT m1) (subTypes m1)
                ]

instance SubTypes M where
  subTypes (C _)     = []
  subTypes (A m0 m1) = mkTypes m0 m1 
  subTypes (D m0 m1) = mkTypes m0 m1 

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

  shrink (C _)   = []
  shrink (A a b) = [a, b]
  shrink (D a b) = [a, b]

-- property: so long as 0 isn't in the divisor, we won't try to divide by 0.
-- It's false: something might evaluate to 0 still.
div1 :: M -> Property
div1 m = divSubTerms m ==> eval m /= Nothing
  where
  -- precondition: no dividand in a subterm can be 0.
  divSubTerms (C _)       = True
  divSubTerms (D _ (C 0)) = False
  divSubTerms (A m0 m1)   = divSubTerms m0 && divSubTerms m1
  divSubTerms (D m0 m1)   = divSubTerms m0 && divSubTerms m1

-- div0 (A _ _) = property False
-- div0 _       = property True

main :: IO ()
main = smartCheck args div1
  where 
  args = scStdArgs { qcArgs = stdArgs 
                                { maxSuccess = 100
                                , maxSize    = 20  }
                   }

---------------------------------------------------------------------------------
