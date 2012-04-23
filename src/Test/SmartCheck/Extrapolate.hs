{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Extrapolate
  ( extrapolate
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Common

import qualified Test.QuickCheck as Q

import Data.Data
import Data.Tree
import Data.List

---------------------------------------------------------------------------------

-- | Test d with arbitrary values replacing its children.  For anything we get
-- 100% failure for, we claim we can generalize it---any term in that hole
-- fails.

-- We extrapolate w.r.t. the original property since extrapolation throws away
-- any values that fail the precondition of the property (i.e., before the
-- Q.==>). XXX
extrapolate :: SubTypes a 
            => Q.Args            -- ^ QuickCheck arguments
            -> a                 -- ^ Current failed value
            -> (a -> Q.Property) -- ^ Original property
            -> [a]               -- ^ Previous failed values
            -> IO ((a -> Q.Property) -> a -> Q.Property)
extrapolate args d origProp ds = do 
  putStrLn ""
  smartPrtLn "Extrapolating ..."
  idxs <- iter args (mkSubstForest d) d origProp (Idx 0 0) []
  if matchesShapes d ds idxs
    then do smartPrtLn "Could not extrapolate a new value; done."
            return (prop' idxs)
    else do smartPrtLn "Extrapolated value:"
            renderWithVars d idxs
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
            then iter' forest (idx { column = 0 
                                   , level  = level idx + 1  
                                   }) 
                       idxs
            else do tries <- iterateArb d idx (Q.maxDiscard args)
                               rate prop
                    case tries of
                      -- None of the tries satisfy prop.  Prevent recurring down
                      -- this tree, since we can generalize (we do this with
                      -- sub, which replaces the subForest with []).
                      FailedProp    -> iter' (sub forest idx Keep False)
                                         (idx { column = column idx + 1 }) 
                                         (idx : idxs)
                      -- Either something satisfied it or the precondition
                      -- couldn't be satisfied.  Recurse down.
                      _             -> iter' forest 
                                         (idx { column = column idx + 1 }) 
                                         idxs

  where
  -- XXX right ratio?  Should I use a user-specified arg?
  rate      = ceiling (sqrt $ fromIntegral (Q.maxDiscard args) :: Float) :: Int
  pts       = breadthLevels forest
  done      = length pts <= level idx
  nextLevel = length (pts !! level idx) <= column idx

  iter' for i is = iter args for d prop i is

---------------------------------------------------------------------------------

-- | Finds any two distinct values that match.  INVARIANT: the ds are all
-- unequal, and d /= any ds.
matchesShapes :: SubTypes a => a -> [a] -> [Idx] -> Bool
matchesShapes d ds idxs = foldl' f False ds
  where
  f True _   = True
  f False d' = matchesShape d d' idxs

-- | Are (1) the value's constructors the same (for algebraic constructors only
-- (e.g., omits Int), (2) all the direct children constructors the same (for
-- algebraic constructors only, (3) ignore differences in all values at holes
-- indexed by the indexes.
matchesShape :: SubTypes a => a -> a -> [Idx] -> Bool
matchesShape a b idxs =
     (if isAlgType (dataTypeOf a) 
        then toConstr a == toConstr b
        else True)
  && repIdxs 

  where
  repIdxs = case foldl' f (Just b) idxs of
              Nothing -> False
              Just b' -> and $ map test $ zip (nextLevel a) (nextLevel b')

  f mb idx = do
    b' <- mb
    v  <- getAtIdx a idx
    replace b' idx v

  nextLevel x = map rootLabel (subTypes x)

  test (SubT x, SubT y)  = 
    if isAlgType (dataTypeOf x) 
      then toConstr x == toConstr y
      else True

---------------------------------------------------------------------------------
-- PrettyPrinting
---------------------------------------------------------------------------------

-- | At each index into d from idxs, replace the whole with a fresh value.
replaceWithVars :: SubTypes a => a -> [Idx] -> [String] -> Tree String
replaceWithVars d idxs vars = 
  foldl' f (mkShowTree d) (zip vars idxs)
  where
  f :: Tree String -> (String, Idx) -> Tree String
  f tree (var, idx) = let forest = sub (subForest tree) idx var False in
                      Node (rootLabel tree) forest

renderWithVars :: SubTypes a => a -> [Idx] -> IO ()
renderWithVars d idxs = do
  putStrLn $ "forall " ++ unwords (take (length idxs) vars) ++ ":"
  putStrLn . drawTree $ replaceWithVars d idxs vars
  where
  vars = map (\(x,i) -> x ++ show i) $ zip (repeat "x") [0::Int ..]

---------------------------------------------------------------------------------
