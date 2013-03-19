{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Comp where

import Test.QuickCheck
import Test.SmartCheck
import Control.Monad

import GHC.Generics
import Data.Typeable

-----------------------------------------------------------------

data I = I Int
  deriving (Eq, Read, Show, Typeable, Generic)

instance Arbitrary I where
  arbitrary = liftM I arbitrary

instance SubTypes I

instance Ord I where
  compare (I i) (I j) = compare i j

data X = X0 I
       | X1 I
  deriving (Eq, Read, Show, Typeable, Generic)

instance Arbitrary X where
  arbitrary = oneof [ liftM X0 arbitrary
                    , liftM X1 arbitrary ]

instance SubTypes X

instance Ord X where
  compare (X0 i) (X0 j) = compare i j
  compare (X0 i) (X1 j) = compare i j
  compare (X1 i) (X0 j) = compare i j
  compare (X1 i) (X1 j) = compare i j

data A = A0 Int
       | A1 Int
  deriving (Eq, Read, Show, Typeable, Generic)

instance Ord A where
  compare (A0 i) (A0 j) = compare i j
  compare (A0 i) (A1 j) = compare i j
  compare (A1 i) (A0 j) = compare i j
  compare (A1 i) (A1 j) = compare i j

instance Arbitrary A where
  arbitrary = oneof [ liftM A0 arbitrary
                    , liftM A1 arbitrary ]

instance SubTypes A

data L = L [Int]
  deriving (Read, Show, Typeable, Generic)

instance Arbitrary L where
  arbitrary = liftM L arbitrary

instance SubTypes L

data B = B L X
  deriving (Read, Show, Typeable, Generic)

instance Arbitrary B where
  arbitrary = liftM2 B arbitrary arbitrary

instance SubTypes B

comp :: X -> X -> A -> Bool
comp a0 a1 _ = a0 < a1

comp2 :: X -> Bool
comp2 a0 = a0 < a0

--------------------------------------------------------------------------------

-- go =:r do
--   r <- quickCheckResult comp
-- :::r  print r

sc :: IO ()
sc = smartCheck scStdArgs comp
