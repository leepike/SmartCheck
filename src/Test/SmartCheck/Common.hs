module Test.SmartCheck.Common
  ( samples
  , iterateArb
  , resultify
  , smartPrefix
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree

import qualified Test.QuickCheck.Gen as Q
import qualified Test.QuickCheck as Q hiding (Result)
import qualified Test.QuickCheck.Property as Q

import System.Random
import Data.List
import Data.Data
import Data.Maybe
import Control.Monad

---------------------------------------------------------------------------------

-- | Make some samples no larger than maxSz of the same type as value a.
samples :: Q.Arbitrary a 
        => a   -- ^ unused; just to type arbitrary.
        -> Int -- ^ Number of tries.
        -> Int -- ^ Maximum size of the structure generated.
        -> IO [a]
samples _ i maxSz = do
  rnd0 <- newStdGen
  when (maxSz < 0) (error "samples: maxSize less than 0.")
  let ls = sort $ take i $ randomRs (0, maxSz) rnd0 -- XXX better distribution.
  let rnds rnd = rnd1 : rnds rnd2 
        where (rnd1,rnd2) = split rnd
  let Q.MkGen m = Q.arbitrary
  return [ (m r n) | (r,n) <- rnds rnd0 `zip` ls ]

---------------------------------------------------------------------------------

-- | Replace the hole in d indexed by idx with a bunch of random values, and
-- test the new d against the property.  Returns the first new d that succeeds.
iterateArb :: (Data a, SubTypes a) 
           => a -> Idx -> Int -> Int
           -> (a -> Q.Property) -> IO (Maybe a)
iterateArb d idx tries sz prop =
  case getAtIdx d idx of
    Nothing -> return Nothing
    Just v  -> do rnds <- mkVals v
                  let res = catMaybes $ map repl rnds
                  -- Catch errors
                  when (length res /= length rnds) (error "iterateArb")
--                  return (find prop res)
                  return (find undefined res)
  where
  mkVals SubT { unSubT = v } = do
    rnds <- samples v tries sz
    return $ map subT rnds

  repl SubT { unSubT = v } = replaceAtIdx d idx v

---------------------------------------------------------------------------------

resultify :: (a -> Q.Property) -> a -> Q.Result
resultify prop a = 
  let Q.MkGen { Q.unGen = f } = prop a :: Q.Gen Q.Prop    in
  let fs = Q.unProp $ f err err        :: Q.Rose Q.Result in
  let (Q.MkRose res _) = fs                               in
  res

  where
  err = error "in propify: should not evaluate."

---------------------------------------------------------------------------------

smartPrefix :: String
smartPrefix = "*** "

---------------------------------------------------------------------------------
