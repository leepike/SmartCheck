{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
data A = A Int16 | Nil
  deriving (Read, Show, Typeable, Generic)

data B = B [A] [A] [A] [A]
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

instance SubTypes B

-- qc/shrink takes over 1m seconds
instance Arbitrary B where
#ifdef feat
  arbitrary = sized uniform
#else
  arbitrary = liftM4 B arbitrary arbitrary
                       arbitrary arbitrary
#endif

#if defined(qcNone) || defined(sc) || defined(feat)
  shrink (B a b c d) = []
#else
  shrink (B a b c d) = [ B w x y z |
                         w <- tk a
                       , x <- tk b
                       , y <- tk c
                       , z <- tk d ]
    where
#ifdef qc10
    sz = 10
#endif
#ifdef qc20
    sz = 20
#endif
    tk x = take sz (shrink x)
#endif

deriveEnumerable ''A
deriveEnumerable ''B

instance Serial B where
  series = cons4 B

add :: [A] -> Int16
add = sum . map (\(A i) -> i)

pre :: B -> Bool
pre (B a b c d) = all (\x -> add x < 16) [a, b, c, d]

post :: B -> Bool
post (B a b c d) = add a + add b + add c + add d < 64

prop_qc :: B -> Property
prop_qc p = pre p ==> post p

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

test :: Int -> IO (Maybe B) -> IO ()
test rnds run = do
  res <- replicateM rnds (test' run)

  app "******************** Results *************"

  let res' = catMaybes res
  let rnds' = length res'
  print res
  let times = fst $ unzip res'
  let szs = snd $ unzip res'
  app $ "Num  : " ++ show rnds'
  app $ "Max  : " ++ show (maximum times)
  app $ "Avg  : " ++ show (sum times / fromIntegral rnds')
  app $ "Med  : " ++ show (sort times !! (rnds' `div` 2))
  app $ "Size : " ++ show (fromIntegral (sum szs) /
                              fromIntegral rnds' :: Double)

  app ""
  app ""

  where app str = appendFile logFile (str ++ "\n")
-- time, size
type Res = Maybe (NominalDiffTime, Int)

test' :: IO (Maybe B) -> IO Res
test' run = do
  start <- getCurrentTime
  res <- run
  stop <- getCurrentTime
  let diff = diffUTCTime stop start
  print diff
  case res of
    Nothing -> return Nothing
    Just r  -> return $ Just (diff, size r)

runSC :: IO (Maybe B)
runSC = do
  res <- runQC stdArgs prop_qc
  case res of
    Nothing -> return Nothing
    Just r  -> liftM Just $ smartRun scStdArgs r prop_qc

-- smartCheck scStdArgs {scMaxExtrap = 20, format = PrintString }prop_qc

--------------------------------------------------------------------------------

size :: B -> Int
size (B a b c d) = length a + length b + length c + length d

logFile :: String
logFile = "init.log"

main :: IO ()
main = do
  [arg] <- getArgs
  let rnds = read arg :: Int
#ifdef feat
  test rnds (runQC stdArgs {maxSuccess = 10000} prop_qc)
#endif
#ifdef sc
  test rnds runSC
#endif
#if defined(qcNone) || defined(qc10) || defined(qc20)
  test rnds (runQC stdArgs prop_qc)
#endif
