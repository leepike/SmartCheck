{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Extrapolate
  ( extrapolate
  ) where

import Test.SmartCheck.Args
import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen
import Test.SmartCheck.Render

import qualified Test.QuickCheck as Q

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
            -> IO ([Idx])
extrapolate args d origProp = do
  putStrLn ""
  smartPrtLn "Extrapolating values ..."
  (_, idxs) <- iter' forest (Idx 0 0) []
  return idxs

  where
  forest = mkSubstForest d True
  iter'  = iter d test next origProp (scMaxDepth args)

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
