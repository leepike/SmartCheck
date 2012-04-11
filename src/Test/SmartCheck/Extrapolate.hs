{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Extrapolate
  ( extrapolate
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Common

import Data.Data
import Data.Tree
import Data.List
import Data.Maybe

---------------------------------------------------------------------------------

-- | Test d with arbitrary values replacing its children.  For anything we get
-- 100% failure for, we claim we can generalize it---any term in that hole
-- fails.
extrapolate :: (Data a, SubTypes a) 
            => SmartArgs -> a -> (a -> Bool) -> IO ()
extrapolate args d prop = do 
  putStrLn ""
  putStrLn $ smartPrefix ++ "Extrapolating ..."
  putStrLn $ smartPrefix ++ "Extrapolated value:"

  idxs <- iter (mkSubstForest d) (Idx 0 0) []
  renderWithVars d idxs

  where
  -- Do a breadth-first traversal of the data, trying to replace items.  When we
  -- find an index we can replace, add it's index to the index list.  Recurse
  -- down the structure, following subtrees that have *not* been replaced.
  iter :: Forest Subst -> Idx -> [Idx] -> IO [Idx]
  iter forest idx idxs =
    if done then return idxs
       else if nextLevel 
              then iter forest (idx { level = level idx + 1 }) idxs
                               -- XXX right ratio?  Should I use a
                               -- user-specified arg?
              else do tries <- iterateArb args d idx (grows args `div` 2) prop
                      if isNothing tries 
                      -- None of the tries satisfy prop.  Prevent recurring down
                      -- this tree, since we can generalize.
                        then iter (sub forest idx Keep)
                                  (idx { column = column idx + 1 }) 
                                  (idx:idxs)
                      -- Can't generalize.
                        else iter forest 
                                  (idx { column = column idx + 1 }) 
                                  idxs
                      
    where
    pts       = breadthLevels forest
    done      = length pts <= level idx
    nextLevel = length (pts !! level idx) <= column idx

---------------------------------------------------------------------------------

replaceWithVars :: SubTypes a => a -> [Idx] -> [String] -> Tree String
replaceWithVars d idxs vars = 
  foldl' f (mkShowTree d) (zip vars idxs)
  where
  f :: Tree String -> (String, Idx) -> Tree String
  f tree (var, idx) = let forest = sub (subForest tree) idx var in
                      Node (rootLabel tree) forest

---------------------------------------------------------------------------------

renderWithVars :: SubTypes a => a -> [Idx] -> IO ()
renderWithVars d idxs = do
  putStrLn $ "forall " ++ unwords (take (length idxs) vars) ++ ":"
  putStrLn . drawTree $ replaceWithVars d idxs vars
  where
  vars = map (\(x,i) -> x ++ show i) $ zip (repeat "x") [0::Int ..]

---------------------------------------------------------------------------------
