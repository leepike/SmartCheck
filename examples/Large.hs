{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Try to generate a very large counterexample.

module Large where

import Test.SmartCheck
import Test.QuickCheck
import GHC.Generics hiding (P, C)
import Data.Typeable
import Control.Monad

-----------------------------------------------------------------

-- Let's try to generate a product type of long lists when all we need is a
-- single element to have a long list.

-- Container so that we don't have base types.
data C = C Int
  deriving (Read, Show, Typeable, Generic)

instance SubTypes C

instance Arbitrary C where
  arbitrary = liftM C arbitrary

data P = P [C] [C] [C] [C]
  deriving (Read, Show, Typeable, Generic)

instance SubTypes P

instance Arbitrary P where
  arbitrary = liftM4 P arbitrary arbitrary arbitrary arbitrary

prop :: P -> Property
prop (P a _ _ _) = property (length a < 20)

main :: IO ()
main = smartCheck scStdArgs { format = PrintString 
                            , scMaxDepth = Just 20
                            } 
         prop


