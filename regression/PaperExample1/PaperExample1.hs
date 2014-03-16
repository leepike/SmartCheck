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
#ifdef small
import Test.LazySmallCheck hiding (Property, test, (==>))
import qualified Test.LazySmallCheck as S
#endif

import GHC.Generics hiding (P, C)
import Data.Typeable
import Control.Monad

import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Data.Time
import System.Environment

#ifdef feat
import Test.Feat
#endif

-----------------------------------------------------------------

-- Let's try to generate a product type of long lists when all we need is a
-- single element to have a long list.

-----------------------------------------------------------------

#ifdef qcjhint
-- So that Int16s aren't shrunk by default arbitrary instances.
newtype J = J { getInt :: Int16 } deriving Show
#endif

#ifdef qcjhint
type I = [J]
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
          go (w', i0', i1', i2', i3') =
            T w' i0' i1' i2' i3'
#endif
#ifdef qcjhint
instance Arbitrary J where
  arbitrary = fmap J arbitrary
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
#ifdef qcjhint
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

  -- Time and size of value.
  appendFile (f ++ "_time.csv") (mkCSV $ plot 200 times)
  appendFile (f ++ "_vals.csv") (mkCSV $ plot 50 szs)

--  appendFile (f ++ "_timelog.csv") (mkCSV $ plot rnds (map (log . (+1)) times))
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

#ifdef smart
-- Little driver since we're not using the SC REPL during testing.
runSC :: IO (Maybe T)
runSC = do
  (res, prop') <- runQC stdArgs prop
  case res of
    Nothing -> return Nothing
    Just r  -> liftM Just $ smartRun scStdArgs r prop'
#endif

runQC' :: Args -> IO (Maybe T)
runQC' args = do
  -- Hack, since runQC refuses to shrink the first arg, which is assumed to be
  -- our SC arg.  However, we want to use it so we don't have to read back in
  -- values.
  let prop' _ = prop
  (res, _) <- runQC args prop'
  return res

--------------------------------------------------------------------------------

size :: T -> Int
size t = sum $ map length (toList t)

logFile :: String
logFile = "regression.log"

main :: IO ()
main = do
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#ifdef feat
  test file rnds (runQC' stdArgs {maxSuccess = 10000})
#endif
#ifdef smart
  test file rnds runSC
#endif
#if defined(qcNone) || defined(qc10) || defined(qc20) || defined(qcjh) || defined (qcjhint)
  test file rnds (runQC' stdArgs)
#endif

#ifdef smart
-- Tester (not part of the benchmark).
smtChk :: IO ()
smtChk = smartCheck scStdArgs { scMaxForall = 20
                              , scMinForall = 25
                              , format = PrintString
                              } prop
#endif
