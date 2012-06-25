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
smartShrink :: forall a. SubTypes a => ScArgs -> a -> (a -> Q.Property) -> IO a
smartShrink args d prop = iter' d (mkForest d) (Idx 0 0) >>= return . fst

  where
  notProp    = Q.expectFailure . prop
  mkForest x = mkSubstForest x ()
  iter' y forest_ idx' = 
    iter y test next notProp forest_ idx' (errorMsg "next-idxs")

  --------------------------------------
  test :: a -> Idx -> IO (Result a)
  test x idx = 
    case maxSize of
      -- Not really failed precondition, but we aren't testing this value.
      -- We'll continue down the tree.
      Nothing     -> return FailedPreCond 
      Just maxVal -> go maxVal

    where
    maxSize :: Maybe Int
    maxSize = let dep = fmap depth forestIdx in
              case dep of 
                Nothing -> Nothing
                Just d'  -> if d' <= 1 then Nothing else Just (d' - 1)
      where
      forestIdx = fmap subForest $ getIdxForest (mkForest x) idx

    go maxVal = do 
      v <- case getAtIdx x idx of
             Nothing  -> return Nothing
             Just v'  -> testHole v'
      case v of
        -- This sees if some subterm directly fails the property.  If so, we'll
        -- take it, if it's well-typed.
        Just v' -> return (Result v')
        -- Otherwise, we'll do maxFailure tests of max, trying to pass the
        -- precondition to find a failure.  We claim to find a failure if some
        -- test satisfies the precondition and satisfies
        --
        -- (Q.expectFailure . originalProp).
        Nothing -> iterateArb x idx (maxFailure args) maxVal notProp

    testHole :: SubT -> IO (Maybe a)
    testHole SubT { unSubT = v } = 
      case cast v :: Maybe a of
        Nothing -> return Nothing
        -- FailedPreCond is just seeding extractResult, a folding operator.
        Just y  -> do res <- extractResult notProp FailedPreCond y
                      case res of
                        -- This is a failed value strictly smaller.  Let's test
                        -- starting from here.
                        Result z      -> return (Just z)
                        FailedPreCond -> return Nothing
                        FailedProp    -> return Nothing

  --------------------------------------

  next :: a -> Result a -> Forest () -> Idx -> [Idx] -> IO (a, [Idx])
  next x res forest idx _ = 
    case res of
      -- Found a try that fails prop.  We'll now test try, and start trying to
      -- reduce from the top!
      Result y      -> iter' y (mkForest y) (Idx 0 0)
      -- Either couldn't satisfy the precondition or nothing satisfied the
      -- property.  Either way, we can't shrink it.
      FailedPreCond -> cont
      FailedProp    -> cont
    where
    cont = iter' x forest idx { column = column idx + 1 }
 
  --------------------------------------
