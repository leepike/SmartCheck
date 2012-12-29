{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Extrapolate
  ( extrapolate
  , matchesShapes
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen
import Test.SmartCheck.Render

import qualified Test.QuickCheck as Q

import Data.Tree
import Data.List

--------------------------------------------------------------------------------

-- | Test d with arbitrary values replacing its children.  For anything we get
-- 100% failure for, we claim we can generalize it---any term in that hole
-- fails.
--
-- We extrapolate if there exists at least one test that satisfies the
-- precondition, and for all tests that satisfy the precondition, they fail.

-- We extrapolate w.r.t. the original property since extrapolation throws away
-- any values that fail the precondition of the property (i.e., before the
-- Q.==>).
extrapolate :: SubTypes a
            => ScArgs            -- ^ Arguments
            -> a                 -- ^ Current failed value
            -> (a -> Q.Property) -- ^ Original property
            -> [a]               -- ^ Previous failed values
            -> IO ([Idx], PropRedux a)
extrapolate args d origProp ds = do
  putStrLn ""
  smartPrtLn "Extrapolating values ..."
  (_, idxs) <- iter' forest (Idx 0 0) []
  return (idxs, prop idxs)

  where
  forest = mkSubstForest d True
  iter'  = iter d test next origProp (scMaxDepth args)
  prop idxs newProp a =
    not (matchesShapes args a (d : ds) idxs) Q.==> newProp a

  -- In this call to iterateArb, we want to claim we can extrapolate iff at
  -- least one test passes a precondition, and for every test in which the
  -- precondition is passed, it fails.  We test values of all possible sizes, up
  -- to Q.maxSize.
  test _ idx = iterateArbIdx d (idx, scMaxDepth args) (scMaxExtrap args)
                 (scMaxSize args) origProp

  -- Control-flow.

  -- None of the tries satisfy prop (but something passed the precondition).
  -- Prevent recurring down this tree, since we can generalize.
  next _ (i, FailedProp) forest' idx idxs
    | scMinExtrap args < i =
        nextIter (forestReplaceChildren forest' idx False) idx (idx : idxs)
  next _ _ forest' idx idxs = nextIter forest' idx idxs

  nextIter f idx = iter' f idx { column = column idx + 1 }

--------------------------------------------------------------------------------

-- | Finds any two distinct values that match.  INVARIANT: the ds are all
-- unequal, and d /= any ds.
matchesShapes :: SubTypes a => ScArgs -> a -> [a] -> [Idx] -> Bool
matchesShapes args d ds idxs = foldl' f False ds
  where
  f True _   = True
  f False d' = matchesShape args d d' idxs

-- | Are the value's constructors the same (for algebraic constructors only
-- (e.g., omits Int)), and all the direct children constructors the same (for
-- algebraic constructors only, while ignoring differences in all values at
-- holes indexed by the indexes.
matchesShape :: SubTypes a => ScArgs -> a -> a -> [Idx] -> Bool
matchesShape args a b idxs = test (subT a, subT b) && repIdxs
  where
  repIdxs = case foldl' f (Just b) idxs of
              Nothing -> False
              Just b' -> all test $ zip (nextLevel a) (nextLevel b')

  f mb idx = do
    b' <- mb
    v  <- getAtIdx a idx (scMaxDepth args)
    replace b' idx v

  nextLevel x = map rootLabel (subTypes x)

  test (SubT x, SubT y)  = baseType x || toConstr x == toConstr y

--------------------------------------------------------------------------------
