{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Extrapolate
  ( extrapolate
  -- YYY
  , matchesShape
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Common
import Test.SmartCheck.Reduce

import qualified Test.QuickCheck as Q

import Data.Data
import Data.Tree
import Data.List
import Data.Maybe
import Control.Monad

---------------------------------------------------------------------------------

-- | Test d with arbitrary values replacing its children.  For anything we get
-- 100% failure for, we claim we can generalize it---any term in that hole
-- fails.
extrapolate :: (Data a, SubTypes a, Read a, Show a, Q.Arbitrary a) 
            => Q.Args -> Maybe a -> (a -> Q.Property) -> IO ()
extrapolate args md prop = do 
  putStrLn ""
  when (isNothing md) (smartPrtLn "No value to extrapolate.")
  when (isJust md) $ do smartPrtLn "Extrapolating ..."
                        smartPrtLn "Extrapolated value:"
                        idxs <- iter (mkSubstForest d) (Idx 0 0) []
                        renderWithVars d idxs
                        d' <- smartRun args (prop' idxs)
                        extrapolate args d' (prop' idxs)

  where
  prop' idxs a = (not $ matchesShape d a idxs) Q.==> prop a

  Just d = md -- use depends on laziness!
  
  -- Do a breadth-first traversal of the data, trying to replace items.  When we
  -- find an index we can replace, add it's index to the index list.  Recurse
  -- down the structure, following subtrees that have *not* been replaced.
  iter :: Forest Subst -> Idx -> [Idx] -> IO [Idx]
  iter forest idx idxs =
    if done then return idxs
       else if nextLevel 
              then iter forest (idx { level = level idx + 1 }) idxs
              else do tries <- iterateArb d idx (Q.maxDiscard args) rate prop
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
    -- XXX right ratio?  Should I use a user-specified arg?
    rate      = ceiling (sqrt $ fromIntegral (Q.maxDiscard args) :: Float) :: Int
    pts       = breadthLevels forest
    done      = length pts <= level idx
    nextLevel = length (pts !! level idx) <= column idx

---------------------------------------------------------------------------------

-- PrettyPrinting

-- | At each index into d from idxs, replace the whole with a fresh value.
replaceWithVars :: SubTypes a => a -> [Idx] -> [String] -> Tree String
replaceWithVars d idxs vars = 
  foldl' f (mkShowTree d) (zip vars idxs)
  where
  f :: Tree String -> (String, Idx) -> Tree String
  f tree (var, idx) = let forest = sub (subForest tree) idx var in
                      Node (rootLabel tree) forest

renderWithVars :: SubTypes a => a -> [Idx] -> IO ()
renderWithVars d idxs = do
  putStrLn $ "forall " ++ unwords (take (length idxs) vars) ++ ":"
  putStrLn . drawTree $ replaceWithVars d idxs vars
  where
  vars = map (\(x,i) -> x ++ show i) $ zip (repeat "x") [0::Int ..]

---------------------------------------------------------------------------------

-- | Does a == b modulo the values located at the idxes?  We compute this by
-- filling in the holes in a with values from b, and seeing if the result is
-- equal.
matchesShape :: (SubTypes a) => a -> a -> [Idx] -> Bool
matchesShape a b idxs =
  case foldl' f (Just b) idxs of
    Nothing -> False
    Just b' -> a == b'

  where
  f mb idx = do
    b' <- mb
    v  <- getAtIdx a idx
    replace b' idx v

---------------------------------------------------------------------------------
