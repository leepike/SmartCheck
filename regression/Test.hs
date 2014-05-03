-- I/O for regression testing.

module Test where

import Control.Monad
import Data.Maybe
import Data.Time
import Data.List

import Test.QuickCheck
import Test.SmartCheck
import Test.SmartCheck.Reduce

--------------------------------------------------------------------------------

test :: FilePath -> Int -> IO Res -> IO ()
test f rnds run = do
  res <- replicateM rnds run
  let res'        = catMaybes res
  let rnds'       = length res'
  let app str     = appendFile logFile (str ++ "\n")
  let avg vals    = sum vals / fromIntegral rnds'
  let med vals    = sort vals !! (rnds' `div` 2)
  let times       = fst $ unzip res'
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
  app $ "std dev : " ++ show (stdDev $ map (fromRational . toRational) times :: Double)
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

type Res = Maybe (Double, Int)

-- test' :: IO (Maybe a) -> (a -> Int) -> IO Res
-- test' run size = do
--   start <- getCurrentTime
--   res <- run
--   stop <- getCurrentTime
--   let diff = diffUTCTime stop start
--   case res of
--     Nothing -> return Nothing
--     Just r  -> return $ Just (fromRational $ toRational diff, size r)

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

logFile :: String
logFile = "regression.log"

data Proxy a = Proxy

proxy :: Proxy a
proxy = Proxy

runQC' :: (Testable prop, Read a)
       => Proxy a -> Args -> prop -> (a -> Int) -> IO Res
runQC' _ args prop size = do
  start <- getCurrentTime
  res   <- quickCheckWithResult args prop
  stop  <- getCurrentTime
  let cex = fmap (read . (!!1)) (getOut res)
  let diff = diffUTCTime stop start
  return $ fmap (\r -> (fromRational $ toRational diff, size r)) cex

getOut :: Result -> Maybe [String]
getOut res = case res of
     Failure{} -> Just $ lines (output res)
     _         -> Nothing

-- Little driver since we're not using the SC REPL during testing.
runSC :: (Arbitrary b, Show b, Testable a, SubTypes b)
      => ScArgs -> (b -> a) -> (b -> Int) -> IO Res
runSC args prop size = do
  start <- getCurrentTime
  (mres, prop') <- runQC (qcArgs args) prop
  res <- case mres of
    Nothing -> return Nothing
    Just r  -> liftM Just $ smartRun args r prop'
  stop  <- getCurrentTime
  let diff = diffUTCTime stop start
  return $ case res of
    Nothing -> Nothing
    Just r  -> Just (fromRational $ toRational diff, size r)

--------------------------------------------------------------------------------
