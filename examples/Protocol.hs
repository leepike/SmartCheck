{-# LANGUAGE DeriveDataTypeable #-}

-- A made up protocol translation.

module Protocol where

import Prelude hiding (last, id)
import Data.Data
import Data.List hiding (last)
import Control.Monad
import Test.QuickCheck
import Test.SmartCheck

-- Input format 

data Input = Input 
  { header :: Header }
  deriving (Data, Typeable, Show, Eq, Read)

data Header = Header 
  { name   :: String
  , id     :: String
  , size   :: Int -- total number of Strings and Ints in the fields.
  , fields :: Fields 
  , misc   :: [String] }
  deriving (Data, Typeable, Show, Eq, Read)

data Fields = Fields 
  { f0   :: Ins
  , f1   :: Ins
  , f2   :: Ins
  , f3   :: Ins
  , f4   :: Ins
  , f5   :: Ins
  , f6   :: Ins
  , f7   :: Ins
  , f8   :: Ins
  , f9   :: Ins
  , fn   :: Ins' }
  deriving (Data, Typeable, Show, Eq, Read)

data Ins = Strs [String] 
         | Ints [Int]
         | Else [String]
  deriving (Data, Typeable, Show, Eq, Read)

data Ins' = Strs' [String] 
         | Ints' [Int]
         | Else' [String]
  deriving (Data, Typeable, Show, Eq, Read)

-- Output format
data Output = Output
  { outSize      :: Int
  , collapseStrs :: [String]
  , collapseInts :: [Int]
  }
  deriving Show

instance Arbitrary Ins where
  arbitrary = oneof [ liftM Strs arbitrary
                    , liftM Ints arbitrary
                    , liftM Else arbitrary ]

instance Arbitrary Ins' where
  arbitrary = oneof [ liftM Strs' arbitrary
                    , liftM Ints' arbitrary
                    , liftM Else' arbitrary ]

instance Arbitrary Fields where
  arbitrary = do arbs <- mapM (\_ -> arbitrary :: Gen Ins) [0..9::Int]
                 last <- arbitrary :: Gen Ins'
                 let f = Fields (arbs !! 0) (arbs !! 1) (arbs !! 2) (arbs !! 3) (arbs !! 4) 
                                (arbs !! 5) (arbs !! 6) (arbs !! 7) (arbs !! 8) (arbs !! 9) last
                 return f

instance Arbitrary Header where
  arbitrary = do f@(Fields a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) <- arbitrary :: Gen Fields
                 let fs = [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]
                 let res = (foldl' (\x y -> x + cnt' y) 0 fs) + cnt'' a10
                 liftM5 Header arbitrary arbitrary (return res) (return f) arbitrary
    where 
    cnt' (Strs ls) = length ls
    cnt' (Ints ls) = length ls
    cnt' _         = 0
    cnt'' (Strs' ls) = length ls
    cnt'' (Ints' ls) = length ls
    cnt'' _          = 0

instance Arbitrary Input where
  arbitrary = liftM Input arbitrary

-- Collect up all the Ins then count the number of Ints and Strs.
-- BUG: Forget to count Ints' but count Strs.
trans :: Input -> Output
trans m = Output sz strs ints 

  where 
  (Fields a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) = (fields . header) m
  fs = [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]
  strs = concat $ getStrs' a10 : map getStrs fs
  ints = concat $ getInts' a10 : map getInts fs 
  sz   = length strs + length ints

  getStrs (Strs ls)   = ls 
  getStrs _           = []
  getStrs' (Strs' ls) = ls 
  getStrs' _          = []

  getInts (Ints ls) = ls 
  getInts _         = []
--  getInts' (Ints' ls) = ls -- XXX BUG!
  getInts' _        = []
  
correctSize :: Input -> Property
correctSize m = 
  property $ (size $ header m) == (outSize $ trans m)

protocolTest :: IO ()
protocolTest = smartCheck args correctSize
  where args = scStdArgs { qcArgs = stdArgs { maxSuccess = 1000 } }
