{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.DataToTree
  ( --subForestPath
--    forestReplaceChop
    forestReplaceChildren
  , getAtIdx
  , replaceAtIdx
  , getIdxForest
  , breadthLevels
  , mkSubstForest 
  , depth
  ) where

import Test.SmartCheck.Types

import Data.Tree
import Data.List
import Data.Maybe
import Data.Typeable

---------------------------------------------------------------------------------
-- Operations on Trees and Forests.
---------------------------------------------------------------------------------

-- | Return the list of values at each level in a Forest Not like levels in
-- Data.Tree (but what I imagined it should have done!).
breadthLevels :: Forest a -> [[a]]
breadthLevels forest = 
  takeWhile (not . null) go
  where
  go = map (getLevel forest) [0..]

---------------------------------------------------------------------------------

-- | Return the elements at level i from a forest.  0-based indexing.
getLevel :: Forest a -> Int -> [a]  
getLevel fs 0 = map rootLabel fs
getLevel fs n = concatMap (\fs' -> getLevel (subForest fs') (n-1)) fs

---------------------------------------------------------------------------------

-- | Get the depth of a Forest.  0-based (an empty Forest has depth 0).
depth :: Forest a -> Int
depth forest = if null ls then 0 else maximum ls
  where
  ls = map depth' forest
  depth' (Node _ [])      = 1
  depth' (Node _ forest') = 1 + depth forest'

---------------------------------------------------------------------------------

-- | How many members are at level i in the Tree?
levelLength :: Int -> Tree a -> Int
levelLength 0 t = length (subForest t)
levelLength n t = sum $ map (levelLength (n-1)) (subForest t)

---------------------------------------------------------------------------------

-- | Get the tree at idx in a forest.  Nothing if the index is out-of-bounds.
getIdxForest :: Forest a -> Idx -> Maybe (Tree a)
getIdxForest forest (Idx (0 :: Int) n) =
  if length forest > n then Just (forest !! n) 
    else Nothing
getIdxForest forest idx              =
  -- Should be a single Just x in the list, holding the value.
  listToMaybe . catMaybes . snd $ acc

  where
  acc = mapAccumL findTree (column idx) (map Just forest)

  l = level idx - 1
  -- Invariant: not at the right level yet.
  findTree :: Int -> Maybe (Tree a) -> (Int, Maybe (Tree a))
  findTree n Nothing  = (n, Nothing)
  findTree n (Just t) =
    let len = levelLength l t in
    if n < 0 -- Already found index
      then (n, Nothing)
      else if n < len -- Big enough to index, so we climb down this one.
             then let t' = getIdxForest (subForest t) (Idx l n) in
                  (n-len, t')
             else (n-len, Nothing)

---------------------------------------------------------------------------------

-- | Returns the value at index idx.  Returns nothing if the index is out of
-- bounds.
getAtIdx :: SubTypes a
         => a     -- ^ Parent value
         -> Idx   -- ^ Index of hole to replace
         -> Maybe SubT
getAtIdx d Idx { level  = l
               , column = c } 
  = if length lev > c then Just (lev !! c) else Nothing
  where
  lev = getLevel (subTypes d) l

---------------------------------------------------------------------------------

data SubStrat = Parent   -- ^ Replace everything in the path from the root to
                         -- here.  Used as breadcrumbs to the value.  Chop the
                         -- subforest.
--              | Chop     -- ^ Replace a value and remove the subforest.
              | Children -- ^ Replace a value and all of its subchildren.
  deriving  (Show, Read, Eq)

---------------------------------------------------------------------------------

forestReplaceParent, forestReplaceChildren :: Forest a -> Idx -> a -> Forest a
--forestReplaceChop, 
forestReplaceParent   = sub Parent
--forestReplaceChop     = sub Chop
forestReplaceChildren = sub Children

---------------------------------------------------------------------------------

sub :: SubStrat -> Forest a -> Idx -> a -> Forest a
-- on right level, and we'll assume correct subtree.
sub strat forest (Idx (0 :: Int) n) a = 
  snd $ mapAccumL f 0 forest
  where
  f i node | i == n = ( i+1, news )  
           | True   = ( i+1, node )

    where  
    news = case strat of
             Parent   -> Node a []
--             Chop     -> Node a []
             Children -> fmap (\_ -> a) (forest !! n)
                         
sub strat forest idx a = 
  snd $ mapAccumL findTree (column idx) forest
  where
  l = level idx - 1
  -- Invariant: not at the right level yet.
  findTree n t = 
    if n < 0 -- Already found index
      then (n, t)
      else if n < len -- Big enough to index, so we climb down this one.
             then (n-len, newTree)
             else (n-len, t)
    where
    len = levelLength l t
    newTree = Node newRootLabel (sub strat (subForest t) (Idx l n) a)
    newRootLabel = case strat of 
                     Parent   -> a 
--                     Chop     -> rootLabel t
                     Children -> rootLabel t

---------------------------------------------------------------------------------
-- Operations on SubTypes.
---------------------------------------------------------------------------------

-- | Make a substitution Forest (all proper children).  Initially we don't
-- replace anything.
mkSubstForest :: SubTypes a => a -> b -> Forest b
mkSubstForest a b = map tMap (subTypes a)
  where tMap = fmap (\_ -> b)

---------------------------------------------------------------------------------

-- | Replace a value at index idx generically in a Tree/Forest generically.
replaceAtIdx :: (SubTypes a, Typeable b)
             => a     -- ^ Parent value
             -> Idx   -- ^ Index of hole to replace
             -> b     -- ^ Value to replace with
             -> Maybe a
replaceAtIdx m idx = replaceChild m (forestReplaceParent subF idx Subst)
  where 
  subF = mkSubstForest m Keep

---------------------------------------------------------------------------------
