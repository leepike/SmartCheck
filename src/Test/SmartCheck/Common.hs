module Test.SmartCheck.Common
  ( samples
  , Result(..)
  , iterateArb
  , extractResult
  , resultify
  , replace
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

-- | Possible results of iterateArb: couldn't satisfy the precondition of a
-- QuickCheck property, failed the property, or satisfied it, with the
-- satisfying value.
data Result a = FailedPreCond
              | FailedProp
              | Result a
  deriving (Show, Read, Eq)

---------------------------------------------------------------------------------

-- | Replace the hole in d indexed by idx with a bunch of random values, and
-- test the new d against the property.  Returns the first new d (the full d but
-- with the hole replaced) that succeeds.
iterateArb :: (Data a, SubTypes a) 
           => a -> Idx -> Int -> Int
           -> (a -> Q.Property) -> IO (Result a)
iterateArb d idx tries sz prop = do
  putStrLn ("iteratearb " ++ show idx)
  putStrLn (show d)
  case getAtIdx d idx of
    Nothing -> error "iterateArb 0"
    Just v  -> do rnds <- mkVals v
                  let res = catMaybes $ map (replace d idx) rnds
                  -- Catch errors that shouldn't ever happen
                  when (length res /= length rnds) (error "iterateArb 1")
                  foldM (extractResult prop) FailedPreCond res
  where
                   
  mkVals SubT { unSubT = v } = do
    rnds <- samples v tries sz
    return $ map subT rnds

---------------------------------------------------------------------------------

extractResult :: (a -> Q.Property) -> Result a -> a -> IO (Result a)
extractResult _    r@(Result _) _ = return r
extractResult prop r            a = do
  res <- resultify prop a 
  let go = case Q.ok res of
             Nothing -> r -- Failed the precondition
             Just b  -> if Q.expect res 
                          then if b then Result a else FailedProp
                          else if b then FailedProp else Result a
  return go

---------------------------------------------------------------------------------

replace :: SubTypes a => a -> Idx -> SubT -> Maybe a
replace d idx SubT { unSubT = v } = replaceAtIdx d idx v

---------------------------------------------------------------------------------

-- | Make a QuickCheck Result by applying a property function to a value.
resultify :: (a -> Q.Property) -> a -> IO Q.Result
resultify prop a = do 
  Q.MkRose r _ <- res fs
  return r

  where
  Q.MkGen { Q.unGen = f } = prop a :: Q.Gen Q.Prop
  fs = Q.unProp $ f err err        :: Q.Rose Q.Result
  res = Q.protectRose . Q.reduceRose

  err = error "in propify: should not evaluate."

---------------------------------------------------------------------------------
