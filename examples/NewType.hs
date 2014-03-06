{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module NewType where

import Test.QuickCheck
import Test.SmartCheck

import Control.Monad
import Data.Word

import GHC.Generics
import Data.Typeable

-----------------------------------------------------------------

prop :: Maybe I -> Bool
prop Nothing      = True
prop (Just (I i)) = (100 > i)

data I = I Word8
  deriving (Eq, Read, Show, Typeable, Generic)

instance Arbitrary I where
  arbitrary    = liftM I arbitrary
  shrink (I i) = map I (shrink i)

instance SubTypes I

sc :: IO ()
sc = smartCheck scStdArgs prop

-----------------------------------------------------------------
