module Test.SmartCheck.Generalize
  ( iter
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen
import Test.SmartCheck.Render

import qualified Test.QuickCheck as Q

import Data.Tree

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
      FailedProp -> iter' (forestStop forest idx)
                      idx { column = column idx + 1 } 
                      (idx : idxs)
      -- Either something satisfied it or the precondition couldn't be
      -- satisfied.  Recurse down.
      _             -> next

---------------------------------------------------------------------------------
