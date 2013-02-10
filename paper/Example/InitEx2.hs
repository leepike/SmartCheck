{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Try to generate a very large counterexample.

module Main where

import Test.SmartCheck
import Test.SmartCheck.Reduce
import Test.QuickCheck
import Test.LazySmallCheck hiding (Property, test, (==>))
import qualified Test.LazySmallCheck as S

import GHC.Generics hiding (P, C)
import Data.Typeable
import Control.Monad

import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Data.Time
import System.Environment

import Test.Feat

-----------------------------------------------------------------

-- Let's try to generate a product type of long lists when all we need is a
-- single element to have a long list.

-- Container so that we don't have base types.

-----------------------------------------------------------------

data N a = N a
  deriving (Read, Show, Typeable, Generic)

type I = [N Int16]

data T = T (N Word8) I I I I
  deriving (Read, Show, Typeable, Generic)

-- SmallCheck --------------------------
enum :: (Enum b, Integral a, Num b) => a -> [b]
enum d = [(-d')..d']
  where d' = fromIntegral d

instance Serial Int16 where
  series = drawnFrom . enum

instance Serial Word8 where
  series = drawnFrom . enum

instance Serial a => Serial (N a) where
  series = cons1 N

instance Serial T where
  series = cons5 T

-- SmallCheck --------------------------

-- SmartCheck --------------------------
instance SubTypes I
instance SubTypes a => SubTypes (N a)
instance SubTypes T
-- SmartCheck --------------------------

instance Arbitrary a => Arbitrary (N a) where
  arbitrary = liftM N arbitrary
  shrink (N i) = map N (shrink i)

-- qc/shrink takes over 1m seconds
instance Arbitrary T where
#ifdef feat
  arbitrary = sized uniform
#else
  arbitrary = liftM5 T arbitrary arbitrary
                       arbitrary arbitrary arbitrary
#endif

#if defined(qcNone) || defined(sc) || defined(feat)
  shrink _ = []
#endif
#ifdef qcjh
  shrink (T w i0 i1 i2 i3) =
    let xs = shrink (w, i0, i1, i2, i3) in
    let go (w', i0', i1', i2', i3') = T w' i0' i1' i2' i3' in
    map go xs
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
deriveEnumerable ''N
deriveEnumerable ''T
-- Feat --------------------------------

toList :: T -> [[Int16]]
toList (T w i0 i1 i2 i3) =
  [go w] : (map . map) go [i0, i1, i2, i3]
  where go (N i) = fromIntegral i

pre :: T -> Bool
pre t = all ((>) 256 . sum) (toList t)

post :: T -> Bool
post t = (sum . concat) (toList t) < 5 * 256

prop :: T -> Property
prop t = pre t ==> post t

-- Smallcheck --------------------------
prop_sc :: T -> Bool
prop_sc t = pre t S.==> post t
-- Smallcheck --------------------------

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

test :: Int -> IO (Maybe T) -> IO ()
test rnds run = do
  res <- replicateM rnds (test' run)
  let res' = catMaybes res
  let rnds' = length res'
  let app str = appendFile logFile (str ++ "\n")
  let avg vals = sum vals / fromIntegral rnds'
  let med vals = sort vals !! (rnds' `div` 2)
  let stdDev vals = sqrt (avg distances)
        where
        distances = map (\x -> (x - m)^2) vals
        m = avg vals

  app "***************"
  print res
  let times = fst $ unzip res'
  let szs :: [Double]
      szs = map fromIntegral (snd $ unzip res')
  app $ "Num     : " ++ show rnds'
  app $ "std dev : " ++ show (stdDev $ map (fromRational . toRational) times)
                     ++ ", " ++ show (stdDev szs)
  app $ "Avg     : " ++ show (avg times) ++ ", " ++ show (avg szs)
  app $ "Med     : " ++ show (med times) ++ ", " ++ show (med szs)

  app ""
  app ""

type Res = Maybe (NominalDiffTime, Int)

test' :: IO (Maybe T) -> IO Res
test' run = do
  start <- getCurrentTime
  res <- run
  stop <- getCurrentTime
  let diff = diffUTCTime stop start
  print diff
  case res of
    Nothing -> return Nothing
    Just r  -> return $ Just (diff, size r)

runSC :: IO (Maybe T)
runSC = do
  res <- runQC stdArgs prop
  case res of
    Nothing -> return Nothing
    Just r  -> liftM Just $ smartRun scStdArgs r prop

--------------------------------------------------------------------------------

size :: T -> Int
size t = sum $ map length (toList t)

logFile :: String
logFile = "init.log"

main :: IO ()
main = do
  [arg] <- getArgs
  let rnds = read arg :: Int
#ifdef feat
  test rnds (runQC stdArgs {maxSuccess = 10000} prop)
#endif
#ifdef sc
  test rnds runSC
#endif
#if defined(qcNone) || defined(qc10) || defined(qc20) || defined(qcjh)
  test rnds (runQC stdArgs prop)
#endif

smtChk :: IO ()
smtChk = smartCheck scStdArgs {scMaxExtrap = 20, scMinExtrap = 25, format = PrintString } prop
