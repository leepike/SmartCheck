{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Try to generate a very large counterexample.

module Large where

import Test.SmartCheck
import Test.QuickCheck
import Test.LazySmallCheck hiding (Property, test, (==>))

import GHC.Generics hiding (P, C)
import Data.Typeable
import Control.Monad

import Data.Int

-----------------------------------------------------------------

-- Let's try to generate a product type of long lists when all we need is a
-- single element to have a long list.

-- Container so that we don't have base types.
data A = A Int16
  deriving (Read, Show, Typeable, Generic)

instance Serial Int16 where
  series d = drawnFrom [(-d')..d']
    where d' = fromIntegral d

instance SubTypes A

instance Arbitrary A where
  arbitrary = liftM A arbitrary
  shrink (A i) = map A (shrink i)

instance Serial A where
  series = cons1 A

data B = B [A] [A] [A] [A]
  deriving (Read, Show, Typeable, Generic)

instance SubTypes B

-- qc/shrink takes over 1m seconds
instance Arbitrary B where
  arbitrary = liftM4 B arbitrary arbitrary arbitrary arbitrary
--  shrink (B a b c d) = [ B w x y z | w <- shrink a, x <- shrink b, y <- shrink c, z <- shrink d ]

instance Serial B where
  series = cons4 B 

add :: [A] -> Int16
add = sum . map (\(A i) -> i)

pre :: B -> Bool
pre (B a b c d) = and $ map pre' [a, b, c, d]
  where
  pre' x = add x < 16

test :: B -> Bool
test (B a b c d) = add a + add b + add c + add d < 64

prop_p :: B -> Property
prop_p p = pre p ==> test p

main :: IO ()
main = smartCheck scStdArgs { extrap = False, constrGen = False } prop_p
  -- smartCheck scStdArgs { format = PrintString 
  --                      , scMaxDepth = Just 20
  --                      } 
  --   prop_p


-- sc :: Int -> IO ()
-- sc n = smallCheck n test

t0 :: Int -> Bool
t0 a = a < 99999999999

---------------------


-- data A = A {int :: Int} deriving Show
-- instance Arbitrary A where
--   arbitrary = liftM A arbitrary
--   shrink (A i) = map A (shrink i)
-- prop :: [A] -> Property
-- prop x = property (sum (map int x) /= 10)

-- instance Serial A where
--   series = cons1 A

-- data B = B A A A A deriving Show
-- instance Arbitrary B where
--   arbitrary = liftM4 B arbitrary arbitrary arbitrary arbitrary
-- instance Serial B where
--   series = cons4 B
-- prop0 :: B -> Bool
-- prop0 (B a b c d) = and $ map f [a, b, c, d]
--   where f (A x) = x < 30

-- data E = E Bool deriving Show

-- instance Serial E where
--   series = cons1 E

-- data F = F E E deriving Show

-- instance Serial F where
--   series = cons2 F

-- prop1 (F (E a) (E b)) = not a || not b
