{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.Matches
  ( matchesShapes
  ) where

import Test.SmartCheck.DataToTree
import Test.SmartCheck.Types
import Test.SmartCheck.SmartGen

import Data.List
import Data.Tree

--------------------------------------------------------------------------------

-- | True if d matches any ds.  Assume all ds are unequal to each other.
matchesShapes :: SubTypes a => a -> [(a,Replace Idx)] -> Bool
matchesShapes d = any (matchesShape d)

--------------------------------------------------------------------------------

-- | At each index that we generalize (either value generalization or
-- constructor generalization), we replace that value from b into a.  At this
-- point, we check for constructor equality between the two values, decending
-- their structures.
matchesShape :: forall a . SubTypes a => a -> (a, Replace Idx) -> Bool
matchesShape a (b, Replace idxVals idxConstrs)
  | toConstr a /= toConstr b = False
  | Just a' <- aRepl         = let x = subTypes a' in
                               let y = subTypes b  in
                               all foldEqConstrs (zip x y)
  | otherwise                = False

  where
  foldEqConstrs :: (Tree SubT, Tree SubT) -> Bool
  foldEqConstrs (Node (SubT l0) sts0, Node (SubT l1) sts1)
    -- Don't need a baseType test, since they don't ever appear in subTypes.
    | toConstr l0 == toConstr l1 = next
    | otherwise                  = False
    where next = all foldEqConstrs (zip sts0 sts1)

  bSub :: Idx -> Maybe SubT
  bSub idx = getAtIdx b idx Nothing

  updateA :: Idx -> a -> Maybe a
  updateA idx d = maybe Nothing (replace d idx) (bSub idx)

  aRepl :: Maybe a
  aRepl = foldl' go (Just a) (idxVals ++ idxConstrs)
    where go ma idx = maybe Nothing (updateA idx) ma

--------------------------------------------------------------------------------
