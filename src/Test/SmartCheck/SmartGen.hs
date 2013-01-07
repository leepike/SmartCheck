{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.SmartGen
  ( iterateArbIdx
  , iterateArb
  , resultify
  , replace
  , iter
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree

import qualified Test.QuickCheck.Gen as Q
import qualified Test.QuickCheck as Q hiding (Result)
import qualified Test.QuickCheck.Property as P

import Prelude hiding (max)
import System.Random
import Data.Tree hiding (levels)

--------------------------------------------------------------------------------

-- | Driver for iterateArb.
iterateArbIdx :: SubTypes a
              => a -> (Idx, Maybe Int) -> Int -> Int
              -> (a -> Q.Property) -> IO (Int, Result a)
iterateArbIdx d (idx, max) tries sz prop =
  case getAtIdx d idx max of
    Nothing  -> errorMsg "iterateArb 0"
    Just ext -> iterateArb d ext idx tries sz prop

-- | Replace the hole in d indexed by idx with a bunch of random values, and
-- test the new d against the property.  Returns the first new d (the full d but
-- with the hole replaced) that succeeds.  "Succeeds" is determined by the call
-- to resultify---if we're expecting failure, then we succeed by getting a value
-- that passes the precondition but fails the property; otherwise we succeed by
-- getting a value that passes the precondition and satisfies the property.  If
-- no value ever satisfies the precondition, then we return FailedPreCond.
-- (Thus, there's an implied linear order on the Result type: FailedPreCond <
-- FailedProp < Result a.)
iterateArb :: forall a. SubTypes a
  => a                  -- ^ Counterexample.
  -> SubT               -- ^ Sub-value in the counterexample.
  -> Idx                -- ^ Index of sub-value.
  -> Int                -- ^ Maximum number of iterations.
  -> Int                -- ^ Maximum size of value to generate.
  -> (a -> Q.Property)  -- ^ Property.
  -> IO (Int, Result a) -- ^ Number of times precondition is passed and returned
                        -- result.
iterateArb d ext idx tries max prop = do
  g <- newStdGen
  iterateArb' (0, FailedPreCond) g 0 0
  where
  newMax SubT { unSubT = v } = valDepth v

  -- Main loop.  We break out if we ever satisfy the property.  Otherwise, we
  -- return the latest value.
  iterateArb' :: (Int, Result a) -> StdGen -> Int -> Int -> IO (Int, Result a)
  iterateArb' (i, res) g try currMax
    -- We've exhausted the number of iterations.
    | try >= tries = return (i, res)
    -- The generated random value is too big.  Start again sampling again with
    -- size at 0.
    | newMax s >= max = iterateArb' (i, res) g0 (try + 1) 0
    | otherwise =
        case replace d idx s of
          Nothing -> errorMsg "iterateArb 1"
          Just d' -> do
            res' <- resultify prop d'
            case res' of
              FailedPreCond -> rec (i, FailedPreCond)
              FailedProp    -> rec (i+1, FailedProp)
              Result x      -> return (i+1, Result x)
    where
    (size, g0) = randomR (0, currMax) g
    s = sample ext g size
    sample SubT { unSubT = v } = newVal v
    rec res' = iterateArb' res' g0 (try + 1)
                 -- XXX what ratio is right to increase size of values?  This
                 -- gives us exponentail growth, but remember we're randomly
                 -- chosing within the range of [0, max], so many values are
                 -- significantly smaller than the max.  Plus we reset the size
                 -- whenever we get a value that's too big.  Note the need for
                 -- (+ 1), since we seed with 0.
                 ((currMax + 1) * 2)

--------------------------------------------------------------------------------

-- | Make a new random value given a generator and a max size.  Based on the
-- value's type's arbitrary instance.
newVal :: forall a. (SubTypes a, Q.Arbitrary a)
       => a -> StdGen -> Int -> SubT
newVal _ g size =
  let Q.MkGen m = Q.resize size (Q.arbitrary :: Q.Gen a) in
  let v = m g size in
  subT v

--------------------------------------------------------------------------------

-- | Put a value v into a another value d at a hole idx, if v is well-typed.
-- Return Nothing if dynamic typing fails.
replace :: SubTypes a => a -> Idx -> SubT -> Maybe a
replace d idx SubT { unSubT = v } = replaceAtIdx d idx v

--------------------------------------------------------------------------------

-- | Make a QuickCheck Result by applying a property function to a value and
-- then get out the Result using our result type.
resultify :: (a -> Q.Property) -> a -> IO (Result a)
resultify prop a = do
  P.MkRose r _ <- res fs
  return $ case P.ok r of -- result of test case (True ==> passed)
             Nothing -> FailedPreCond -- Failed precondition (discard)
             Just b  ->  if notExceptionFail r then get b r
                           -- If failed because of an exception, just say we
                           -- failed.
                           else FailedProp

  where
  get b r |     b &&      P.expect r    = Result a -- expected to pass and we
                                                   -- did
          | not b && not (P.expect r)   = Result a -- expected failure and got
                                                   -- it
          | otherwise                   = FailedProp -- We'll just discard it.

  Q.MkGen { Q.unGen = f } = prop a :: Q.Gen P.Prop
  fs  = P.unProp $ f err err       :: P.Rose P.Result
  res = P.protectRose . P.reduceRose

  -- XXX A hack!  Means we failed the property because it failed, not because of
  -- an exception (i.e., with partial function tests).
  notExceptionFail r = let e = P.reason r in
                       e == "Falsifiable" || e  == ""

  err = errorMsg "resultify: should not evaluate."

--------------------------------------------------------------------------------

type Test a b = a -> Idx -> IO b
type Next a b = a -> b -> Forest Bool -> Idx -> [Idx] -> IO (a, [Idx])

-- Do a breadth-first traversal of the data.  First, we find the next valid
-- index we can use.  Then we apply our test function, passing the result to our
-- next function.
iter :: SubTypes a
     => a                 -- ^ Failed value
     -> Test a b          -- ^ Test to use
     -> Next a b          -- ^ What to do after the test
     -> (a -> Q.Property) -- ^ Property
     -> Maybe Int         -- ^ Max depth to analyze
     -> Forest Bool       -- ^ Only evaluate at True indexes.
     -> Idx               -- ^ Starting index to extrapolate
     -> [Idx]             -- ^ List of generalized indices
     -> IO (a, [Idx])
iter d test nxt prop maxLevel forest idx idxs
  | done      = return (d, idxs)
  | nextLevel = iter'
  | atFalse   = iter' -- Must be last check or !! index below may be out of
                      -- bounds!
  | otherwise = do tries <- test d idx
                   nxt d tries forest idx idxs
  where
  -- Location is w.r.t. the forest, not the original data value.
  levels     = breadthLevels forest
  done       = length levels <= l || tooDeep l maxLevel
  nextLevel  = length (levels !! l) <= column idx
  atFalse    = not $ (levels !! l) !! column idx
  iter'      = iter d test nxt prop maxLevel forest
                 idx { level = l + 1, column = 0 } idxs
  l          = level idx

--------------------------------------------------------------------------------

-- | Get the maximum depth of a value, where depth is measured in the maximum
-- depth of the tree representation, not counting base types (defined in
-- Types.hs).
valDepth :: SubTypes a => a -> Int
valDepth d = depth (mkSubstForest d True)

--------------------------------------------------------------------------------
