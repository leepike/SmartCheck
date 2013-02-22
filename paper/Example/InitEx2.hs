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
  shrink (T w i0 i1 i2 i3) = map go xs
    where xs = shrink (w, i0, i1, i2, i3)
          go (w', i0', i1', i2', i3') =
            T w' i0' i1' i2' i3'
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

test :: FilePath -> Int -> IO (Maybe T) -> IO ()
test f rnds run = do
  res <- replicateM rnds (test' run)
  let res'        = catMaybes res
  let rnds'       = length res'
  let app str     = appendFile logFile (str ++ "\n")
  let avg vals    = sum vals / fromIntegral rnds'
  let med vals    = sort vals !! (rnds' `div` 2)
  let times = fst $ unzip res'
  let szs :: [Double]
      szs = map fromIntegral (snd $ unzip res')
  let stdDev vals = sqrt (avg distances)
        where
        distances = map (\x -> (x - m)^(2::Integer)) vals
        m = avg vals
  let percentile n vals = sort vals !! ((rnds' `div` 100) * n)
  -- http://en.wikipedia.org/wiki/Median_absolute_deviation
  let medAbsDev vals = med (map dist vals)
        where dist v = abs (v - median)
              median = med vals

  app "***************"
  print res
  app $ "Num     : " ++ show rnds'
  app $ "std dev : " ++ show (stdDev $ map (fromRational . toRational) times)
                     ++ ", " ++ show (stdDev szs)
  app $ "Avg     : " ++ show (avg times) ++ ", " ++ show (avg szs)
  app $ "Med     : " ++ show (med times) ++ ", " ++ show (med szs)
  app $ "75%     : " ++ show (percentile 75 times) ++ ", " ++ show (percentile 75 szs)
  app $ "95%     : " ++ show (percentile 95 times) ++ ", " ++ show (percentile 95 szs)
  app $ "99%     : " ++ show (percentile 99 times) ++ ", " ++ show (percentile 99 szs)
  app $ "MAD     : " ++ show (medAbsDev times) ++ ", " ++ show (medAbsDev szs)
  app ""
  app ""

  appendFile (f ++ "_time.csv") (mkCSV $ plot 200 times)
--  appendFile (f ++ "_timelog.csv") (mkCSV $ plot rnds (map (log . (+1)) times))
  appendFile (f ++ "_vals.csv") (mkCSV $ plot 50 szs)
--  appendFile (f ++ "_valslog.csv") (mkCSV $ plot rnds (map (log . (+100)) szs))

-- For gnuplot ---------------------------------------
mkCSV :: Show a => [(a,a)] -> String
mkCSV [] = "\n"
mkCSV ((x,y):rst) = show x ++ ", " ++ show y ++ "\n" ++ mkCSV rst

-- Make 100 compartments to put data in.
plot :: Double -> [Double] -> [(Double,Double)]
plot comparts vals = filter (\(_,n) -> n /= 0.0) $ cz vs (min' + compartSz, 0)
  where
  vs          = sort vals
  (min',max') = (head vs, last vs)
  compartSz   = (max' - min') / comparts

  -- Count how many values are in each compartment.  (1st element is top of
  -- compartment, 2nd is how many seen.)
  cz :: [Double] -> (Double,Double) -> [(Double,Double)]
  cz [] _ = []
  cz (v:vs') (c,n) | v <= c    = cz vs' (c,n+1)
                   | otherwise = (c,n) : cz (v:vs') (c + compartSz, 0)

type Res = Maybe (Double, Int)

test' :: IO (Maybe T) -> IO Res
test' run = do
  start <- getCurrentTime
  res <- run
  stop <- getCurrentTime
  let diff = diffUTCTime stop start
  print diff
  case res of
    Nothing -> return Nothing
    Just r  -> return $ Just (fromRational $ toRational diff, size r)

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
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#ifdef feat
  test file rnds (runQC stdArgs {maxSuccess = 10000} prop)
#endif
#ifdef sc
  test file rnds runSC
#endif
#if defined(qcNone) || defined(qc10) || defined(qc20) || defined(qcjh)
  test file rnds (runQC stdArgs prop)
#endif

smtChk :: IO ()
smtChk = smartCheck scStdArgs {scMaxExtrap = 20, scMinExtrap = 25, format = PrintString } prop
