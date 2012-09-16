{-# LANGUAGE ScopedTypeVariables #-} 

module Test.SmartCheck.Reduce
  (smartRun
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.SmartGen 
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Render

import qualified Test.QuickCheck as Q

import Data.Typeable
import Data.Tree
import Data.Maybe

---------------------------------------------------------------------------------

-- Smarter than shrinks.  Does substitution.  m is a value that failed QC that's
-- been shrunk.  We substitute successive children with strictly smaller (and
-- increasingly larger) randomly-generated values until we find a failure, and
-- return that result.  (We call smartShrink recursively.)
smartRun :: SubTypes a => ScArgs -> a -> (a -> Q.Property) -> IO a
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
smartShrink :: forall a. SubTypes a => ScArgs -> a -> (a -> Q.Property) -> IO a
smartShrink args d prop = 
  iter' d (mkForest d) (Idx 0 0) >>= return . fst

  where
  mkForest x = mkSubstForest x True
  notProp = Q.expectFailure . prop

  iter' x forest_ idx' = 
    iter x test next notProp forest_ idx' (errorMsg "next-idxs")

  -------------------------------------- 

  -- next tells the iter what to do after running a test.
  next :: a -> Maybe a -> Forest Bool -> Idx -> [Idx] -> IO (a, [Idx])
  next x res forest idx _ = 
    case res of
      -- Found a try that fails prop.  We'll now test try, and start trying to
      -- reduce from the top!
      Just y  -> iter' y (mkForest y) (Idx 0 0)
      -- Either couldn't satisfy the precondition or nothing satisfied the
      -- property.  Either way, we can't shrink it.
      Nothing -> iter' x forest idx { column = column idx + 1 }

  -------------------------------------- 

  -- Our test function.  First, we'll see if we can just return the hole at idx,
  -- assuming it's (1) well-typed and (2), fails the test.  Otherwise, we'll
  -- test x by replacing values at idx against (Q.expectFailure . prop).  Make
  -- sure that values generated are strictly smaller than the value at
  -- idx.
  test :: a -> Idx -> IO (Maybe a)
  test x idx = do
    hole <- testHole (getAtIdx x idx)
    if isJust hole then return hole
      else do r <- iterateArb x idx (maxFailure args) (maxSize x idx) notProp
              return $ case r of
                         Result a -> Just a
                         FailedPreCond -> Nothing
                         FailedProp    -> Nothing

    where
    -- -- XXX debuging
    -- sh (Just SubT { unSubT = v }) = show v

    testHole :: Maybe SubT -> IO (Maybe a)
    testHole Nothing = return Nothing
    testHole (Just SubT { unSubT = v }) = 
      case cast v :: Maybe a of
        Just v' -> extractAndTest v'
        Nothing -> return Nothing
      where
      extractAndTest :: a -> IO (Maybe a)
      extractAndTest y = do 
        res <- resultify notProp y
        return $ case res of
                   FailedPreCond -> Nothing
                   FailedProp    -> Nothing
                   Result n      -> Just n

---------------------------------------------------------------------------------

-- | Get the maximum depth of d's subforest at idx.  Intuitively, it's the
-- maximum number of constructors you have *below* the constructor at idx.  So
-- for a unary constructor C, the value [C, C, C]

-- (:) C 
--   (:) C
--     (:) C []

-- At (Idx 0 0) in v, we're at C, so maxSize v (Idx 0 0) == 0.
-- At (Idx 0 1) in v, we're at (C : C : []), so maxSize v (Idx 0 1) == 2, since
-- we have the constructors :, C (or :, []) in the longest path underneath.
-- Base-types have maxSize 0.  So maxSize [1,2,3] idx == 0 for any idx.
-- Note that if we have maxSize d idx == 0, then it is impossible to construct a
-- *structurally* smaller value at hole idx.
maxSize :: SubTypes a => a -> Idx -> Int
maxSize d idx = maybe 0 id (fmap depth forestIdx)
  where
  forestIdx :: Maybe [Tree Bool]
  forestIdx = fmap subForest $ getIdxForest (mkSubstForest d True) idx

---------------------------------------------------------------------------------
