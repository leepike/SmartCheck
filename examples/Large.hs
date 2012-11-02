{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Try to generate a very large counterexample.

module Large where

import Test.SmartCheck
import Test.QuickCheck
import Test.LazySmallCheck hiding (Property, test)
import qualified Test.LazySmallCheck as S

import GHC.Generics hiding (P, C)
import Data.Typeable
import Control.Monad
import Data.List

-----------------------------------------------------------------

-- Let's try to generate a product type of long lists when all we need is a
-- single element to have a long list.

-- Container so that we don't have base types.
data C = C Int
  deriving (Read, Show, Typeable, Generic)

instance SubTypes C

instance Arbitrary C where
  arbitrary = liftM C arbitrary
  shrink (C i) = map C (shrink i)

instance Serial C where
  series = cons1 C

data P = P [C] [C] [C] [C]
  deriving (Read, Show, Typeable, Generic)

instance SubTypes P

instance Arbitrary P where
  arbitrary = liftM4 P arbitrary arbitrary arbitrary arbitrary
--  shrink (P a b c d) = [ P w x y z | w <- shrink a, x <- shrink b, y <- shrink c, z <- shrink d ]

instance Serial P where
  series = cons4 P 

sumC :: [C] -> Int
sumC = foldl' (\acc (C c) -> acc + c) 0 

test :: P -> Bool
test (P a b _ _) = sumC a - sumC b < 4096

prop :: P -> Property
prop = property . test

main :: IO ()
main = smartCheck scStdArgs { format = PrintString 
                            , scMaxDepth = Just 20
                            } 
         prop




t0 :: Int -> Bool
t0 a = a < 99999999999
