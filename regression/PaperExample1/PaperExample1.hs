{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Try to generate a very large counterexample.

module Main where

import Test
import Test.SmartCheck
import Test.SmartCheck.Reduce
import Test.QuickCheck
#ifdef small
import Test.LazySmallCheck hiding (Property, test, (==>))
import qualified Test.LazySmallCheck as S
#endif

import GHC.Generics hiding (P, C)
import Data.Typeable

import Data.Int
import Data.Word
import Control.Monad
import System.Environment

#ifdef feat
import Test.Feat
#endif

-----------------------------------------------------------------

#if defined(qcjhint) || defined(qcNone) || defined(qc10) || defined(qc20)
-- So that Int16s aren't shrunk by default arbitrary instances.
newtype J = J { getInt :: Int16 } deriving Show
type I = [J]
instance Arbitrary J where
  arbitrary = fmap J arbitrary

#else
type I = [Int16]
#endif

data T = T (Word8) I I I I
  deriving (Show, Typeable, Generic)

-- SmallCheck --------------------------
#ifdef small
enum :: (Enum b, Integral a, Num b) => a -> [b]
enum d = [(-d')..d']
  where d' = fromIntegral d

instance Serial Int16 where
  series = drawnFrom . enum

instance Serial Word8 where
  series = drawnFrom . enum

instance Serial T where
  series = cons5 T
#endif
-- SmallCheck --------------------------

-- SmartCheck --------------------------
#ifdef smart
instance SubTypes I
instance SubTypes T
#endif
-- SmartCheck --------------------------

-- qc/shrink takes over 1m seconds
instance Arbitrary T where
#ifdef feat
  arbitrary = sized uniform
#else
  arbitrary = liftM5 T arbitrary arbitrary
                       arbitrary arbitrary arbitrary
#endif

#if defined(qcNone) || defined(feat)
  shrink _ = []
#endif
#if defined(qcjh) || defined(qcjhint)
  shrink (T w i0 i1 i2 i3) = map go xs
    where xs = shrink (w, i0, i1, i2, i3)
          go (w', i0', i1', i2', i3') = T w' i0' i1' i2' i3'
#endif
#if defined(qc10) || defined(qc20)
  shrink (T w i0 i1 i2 i3) =
    [ T a b c d e | a <- tk w
                  , b <- tk i0, c <- tk i1
                  , d <- tk i2, e <- tk i3 ]
    where
#ifdef qc10
    sz = 10
#endif
#ifdef qc20
    sz = 20
#endif
    tk x = take sz (shrink x)
#endif

-- Feat --------------------------------
#ifdef feat
deriveEnumerable ''T
#endif
-- Feat --------------------------------

toList :: T -> [[Int16]]
toList (T w i0 i1 i2 i3) =
#if defined(qcjhint) || defined(qcNone) || defined(qc10) || defined(qc20)
  [fromIntegral w] : (map . map) (fromIntegral . getInt) [i0, i1, i2, i3]
#else
  [fromIntegral w] : (map . map) fromIntegral [i0, i1, i2, i3]
#endif


pre :: T -> Bool
pre t = all ((>) 256 . sum) (toList t)

post :: T -> Bool
post t = (sum . concat) (toList t) < 5 * 256

prop :: T -> Property
prop t = pre t ==> post t

-- Smallcheck --------------------------
#ifdef small
prop_small :: T -> Bool
prop_small t = pre t S.==> post t
#endif
-- Smallcheck --------------------------

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

size :: T -> Int
size t = sum $ map length (toList t)

main :: IO ()
main = do
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#ifdef feat
  test file rnds (runQC' stdArgs {maxSuccess = 10000} prop) size
#endif
#ifdef smart
  test file rnds (runSC scStdArgs prop) size
#endif
#if defined(qcNone) || defined(qc10) || defined(qc20) || defined(qcjh) || defined (qcjhint)
  test file rnds (runQC' stdArgs prop) size
#endif

#ifdef smart
-- Tester (not part of the benchmark).
smtChk :: IO ()
smtChk = smartCheck scStdArgs { scMaxForall = 20
                              , runForall   = True
                              , scMinForall = 25
                              , format = PrintString
                              } prop
#endif
