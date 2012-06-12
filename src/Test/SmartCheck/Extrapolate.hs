{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Extrapolate
  ( extrapolate
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen
import Test.SmartCheck.Render

import qualified Test.QuickCheck as Q

import Data.Tree
import Data.List

---------------------------------------------------------------------------------

-- | Test d with arbitrary values replacing its children.  For anything we get
-- 100% failure for, we claim we can generalize it---any term in that hole
-- fails.

-- We extrapolate w.r.t. the original property since extrapolation throws away
-- any values that fail the precondition of the property (i.e., before the
-- Q.==>).

-- We extrapolate if there exists at least one test that satisfies the
-- precondition, and for all tests that asatisfy the precondition, they fail.
extrapolate :: SubTypes a
            => ScArgs            -- ^ Arguments
            -> a                 -- ^ Current failed value
            -> (a -> Q.Property) -- ^ Original property
            -> [a]               -- ^ Previous failed values
            -> IO ((a -> Q.Property) -> a -> Q.Property)
extrapolate args d origProp ds = do 
  putStrLn ""
  smartPrtLn "Extrapolating ..."
  idxs <- iter (qcArgs args) (mkSubstForest d) d origProp (Idx 0 0) []
  if matchesShapes d ds idxs
    then do smartPrtLn "Could not extrapolate a new value; done."
            return (prop' idxs)
    else do smartPrtLn "Extrapolated value:"
            renderWithVars (treeShow args) d idxs
            return (prop' idxs)

  where
  prop' idxs newProp a = 
    (not $ matchesShapes a (d:ds) idxs) Q.==> newProp a

---------------------------------------------------------------------------------

-- Do a breadth-first traversal of the data, trying to replace holes.  When we
-- find an index we can replace, add it's index to the index list.  Recurse down
-- the structure, following subtrees that have *not* been replaced.
iter :: SubTypes a 
     => Q.Args -> Forest Subst -> a 
     -> (a -> Q.Property) -> Idx -> [Idx] -> IO [Idx]
iter args forest d prop idx idxs = 
  if done then return idxs
     else if nextLevel 
            then iter' forest (idx { level  = level idx + 1  
                                   , column = 0 })
                       idxs
            else case getIdxForest forest idx of
                   Just (Node Keep _) -> runTest
                   _                  -> next

  where
  pts       = breadthLevels forest
  done      = length pts <= level idx
  nextLevel = length (pts !! level idx) <= column idx

  next = iter' forest 
           idx { column = column idx + 1 }
           idxs

  iter' frst = iter args frst d prop

  runTest = do 
    -- In this call to iterateArb, we want to claim we can extrapolate iff at
    -- least one test passes a precondition, and for every test in which the
    -- precondition is passed, it fails.  We test values of all possible sizes,
    -- up to Q.maxSize.
    tries <- iterateArb d idx (Q.maxSuccess args) (Q.maxSize args) prop
    case tries of
      -- None of the tries satisfy prop.  Prevent recurring down this tree,
      -- since we can generalize (we do this with sub, which replaces the
      -- subForest with []).
      FailedProp    -> iter' (forestStop forest idx)
                         idx { column = column idx + 1 } 
                         (idx : idxs)
      -- Either something satisfied it or the precondition couldn't be
      -- satisfied.  Recurse down.
      _             -> next

---------------------------------------------------------------------------------

-- | Finds any two distinct values that match.  INVARIANT: the ds are all
-- unequal, and d /= any ds.
matchesShapes :: SubTypes a => a -> [a] -> [Idx] -> Bool
matchesShapes d ds idxs = foldl' f False ds
  where
  f True _   = True
  f False d' = matchesShape d d' idxs

-- | Are the value's constructors the same (for algebraic constructors only
-- (e.g., omits Int)), and all the direct children constructors the same (for
-- algebraic constructors only, while ignoring differences in all values at
-- holes indexed by the indexes.
matchesShape :: SubTypes a => a -> a -> [Idx] -> Bool
matchesShape a b idxs = test (subT a, subT b) && repIdxs 
  where
  repIdxs = case foldl' f (Just b) idxs of
              Nothing -> False
              Just b' -> and . map test $ zip (nextLevel a) (nextLevel b')

  f mb idx = do
    b' <- mb
    v  <- getAtIdx a idx
    replace b' idx v

  nextLevel x = map rootLabel (subTypes x)

  test (SubT x, SubT y)  = baseType x || toConstr x == toConstr y

---------------------------------------------------------------------------------
