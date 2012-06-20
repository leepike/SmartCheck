{-# LANGUAGE ScopedTypeVariables #-} 

module Test.SmartCheck.Reduce
  (smartRun
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.SmartGen
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Render

import qualified Test.QuickCheck as Q
import Data.Tree
import Data.Typeable

---------------------------------------------------------------------------------

-- Smarter than shrinks.  Does substitution.  m is a value that failed QC that's
-- been shrunk.  We substitute successive children with strictly smaller (and
-- increasingly larger) randomly-generated values until we find a failure, and
-- return that result.  (We call smartShrink recursively.)
smartRun :: SubTypes a
         => ScArgs -> a -> (a -> Q.Property) -> IO a
smartRun args res prop = do
  putStrLn ""
  smartPrtLn "Smart Shrinking ... "
  new <- smartShrink args res prop
  smartPrtLn "Smart-shrunk value:"
  print new
  return new

---------------------------------------------------------------------------------

-- | Breadth-first traversal of d, trying to shrink it with *strictly* smaller
-- children.  We replace d whenever a successful shrink is found and try again.
smartShrink :: SubTypes a => ScArgs -> a -> (a -> Q.Property) -> IO a
smartShrink args d prop = iterReduce args d (Idx 0 0) notProp
  where
  notProp = Q.expectFailure . prop

iterReduce :: SubTypes a => ScArgs -> a -> Idx -> (a -> Q.Property) -> IO a
iterReduce args d idx prop = 
  if done then return d
    else if nextLevel 
           then iterReduce args d idx { column = 0
                                      , level  = level idx + 1 }
                           prop
           else case maxSize of
                  Nothing -> iterReduce args d (idx { column = column idx + 1 })
                               prop
                  -- XXX We could shrink base values, but I'm not sure if
                  -- it's worth it.  Doesn't affect extrapolation or make
                  -- counter-examples more readable.
                  -- then case getAtIdx d idx of
                  --        Nothing -> iterReduce args d 
                  --                     (idx { column = column idx + 1 }) 
                  --                     prop
                  --        Just v  -> mkVals v

                  -- Invariant: ms should be => 1.                
                  Just ms -> mkTry args d idx prop ms
  where
  -- Extract a tree from a forest and make sure it's big enough to test.
  maxSize   = case getIdxForest forest idx of
                Nothing -> Nothing
                Just t  -> let dep = depth (subForest t) in
                           if dep <= 1 then Nothing
                             else Just (dep-1)

  forest    = mkSubstForest d
  pts       = breadthLevels forest
  done      = length pts <= level idx
  nextLevel = length (pts !! level idx) <= column idx

---------------------------------------------------------------------------------

mkTry :: forall a. SubTypes a
      => ScArgs -> a -> Idx -> (a -> Q.Property) -> Int -> IO a
mkTry args d idx prop maxSize = do
  v <- mv
  case v of
    -- This sees if some subterm directly fails the property.  If so, we'll take
    -- it, if it's well-typed.
    Just v' -> iterReduce args v' (Idx 0 0) prop
    Nothing -> do 
      -- We'll at least maxFailure tests of maxSize, trying to pass the
      -- precondition to find a failure.  We claim to find a failure if some
      -- test satisfies the precondition and satisfies
      --
      -- (Q.expectFailure . originalProp).
      try <- iterateArb d idx (maxFailure args) maxSize prop
      case try of
        -- Found a try that fails prop.  We'll now test try, and start trying to
        -- reduce from the top!  
        Result x -> iterReduce args x (Idx 0 0) prop
        -- Either couldn't satisfy the precondition or nothing satisfied the
        -- property.  Either way, we can't shrink it.
        _        -> iterReduce args d (idx { column = column idx + 1 }) prop

  where
  mv = case getAtIdx d idx of
         Nothing -> errorMsg "unexpected failure: getAtIdx mkTry"
         Just v  -> testHole v

  testHole :: SubT -> IO (Maybe a)
  testHole SubT { unSubT = v } = 
    case cast v :: Maybe a of
      Nothing -> return Nothing
      -- FailedPreCond is just seeding extractResult, a folding operator.
      Just x  -> do res <- extractResult prop FailedPreCond x 
                    case res of
                      -- This is a failed value strictly smaller.  Let's test
                      -- starting from here.
                      Result y -> return (Just y)
                      _        -> return Nothing
  
---------------------------------------------------------------------------------
