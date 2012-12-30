{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Try to generate a very large counterexample.

module Paper where

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

-----------------------------------------------------------------

-- Let's try to generate a product type of long lists when all we need is a
-- single element to have a long list.

-- Container so that we don't have base types.

-----------------------------------------------------------------
data A = A Int16
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
  arbitrary = liftM4 B arbitrary arbitrary
                       arbitrary arbitrary
  shrink (B a b c d) =
    if defShrink
      then [ B w x y z |
             w <- tk a
           , x <- tk b
           , y <- tk c
           , z <- tk d ]
      else []
    where tk x = take 10 (shrink x)

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

prop_sc :: B -> Bool
prop_sc p = pre p S.==> post p

test :: IO (Maybe B) -> IO ()
test run = do
  [arg] <- getArgs
  let rnds = read arg :: Int
  res <- replicateM rnds (test' run)

  putStrLn ""
  putStrLn "******************** Results *************"
  putStrLn ""

  let res' = catMaybes res
  let rnds' = length res'
  print res
  let times = fst $ unzip res'
  let szs = snd $ unzip res'
  putStrLn $ "Num  : " ++ show rnds'
  putStrLn $ "Max  : " ++ show (maximum times)
  putStrLn $ "Avg  : " ++ show (sum times / fromIntegral rnds')
  putStrLn $ "Med  : " ++ show (sort times !! (rnds' `div` 2))
  putStrLn $ "Size : " ++ show (fromIntegral (sum szs) / fromIntegral rnds' :: Double)

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

-- -- XXX note we're parsing out QC results, so slower than QC.
-- testsc ::
-- testsc = do
--   start <- getCurrentTime
--   res <- runQC scStdArgs prop
--test $  >>= smartRun scStdArgs XXX prop)

-- smartCheck scStdArgs { extrap = False, constrGen = False } prop_p

runSC :: IO (Maybe B)
runSC = do
  res <- runQC stdArgs prop_qc
  case res of
    Nothing -> return Nothing
    Just r  -> liftM Just $ smartRun scStdArgs r prop_qc

--------------------------------------------------------------------------------

size :: B -> Int
size (B a b c d) = length a + length b + length c + length d

defShrink :: Bool
defShrink = False

main :: IO ()
main = test $ runQC stdArgs prop_qc
--  test runSC

  -- _ <- test' $ smallCheck 7 prop_sc >> return (Just $ B [] [] [] [])
  -- return ()


{-
-- RESULTS

-- maybe a 2x noise in results

----------------------------------------
QC, no shrinking:
----------------------------------------

Num  : 100
Max  : 0.154356s
Avg  : 0.0733841s
Med  : 0.07041s
Size : 70.25

----------------------------------------
QC, shrinking:
----------------------------------------

-- take 10
Num  : 100
Max  : 21.511811s
Avg  : 1.21184463s
Med  : 0.48234s
Size : 33.71

-- take 20
Num  : 100
Max  : 125.366509s
Avg  : 3.79998622s
Med  : 0.516224s
Size : 32.34

----------------------------------------
SC:
----------------------------------------
Num  : 100
Max  : 1.961384s
Avg  : 0.30087232s
Med  : 0.24315s
Size : 5.35

-}
