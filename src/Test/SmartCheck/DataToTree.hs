{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.SmartCheck.DataToTree
  ( subForestPath
--  , forestReplace
  , forestReplaceChop
  , forestStop
  , getAtIdx
  , replaceAtIdx
  , getIdxForest
  , breadthLevels
  , mkSubstForest
  , depth
  ) where

import Test.SmartCheck.Types

import Control.Monad.State 
import Data.Tree
import Data.Data
import Data.List
import Data.Maybe

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
getIdxForest forest (Idx (0::Int) n) =
  if length forest > n then Just (forest !! n) else Nothing
getIdxForest forest idx              =
  -- Should be a single Just x in the list, holding the value.
  listToMaybe . catMaybes . snd $ mapAccumL findTree (column idx) (map Just forest)
  where
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
  lev = getLevel (forestRep d) l

---------------------------------------------------------------------------------

-- | Replace a tree at index Idx in a Forest.  Return the original if the index
-- is out of range.  All subforests are removed.  Additionally, every rootLabel
-- in the path to Index is replaced with a.
subForestPath :: Forest a -> Idx -> a -> Forest a
subForestPath = sub Path

---------------------------------------------------------------------------------

-- | Replace a tree at index Idx in a Forest.  Return the original if the index
-- is out of range.  All subforests are removed.
forestReplaceChop :: Forest a -> Idx -> a -> Forest a
forestReplaceChop = sub Chop

---------------------------------------------------------------------------------

-- | Replace a tree at index Idx in a Forest.  Return the original if the
-- index is out of range.  Replace the subforest of Idx with the Substs.
forestStop :: Forest Subst -> Idx -> Forest Subst
forestStop f idx = sub ReplaceSubs f idx Subst

---------------------------------------------------------------------------------

data SubStrat = Path | ReplaceSubs | Chop
  deriving  (Show, Read, Eq)

sub :: SubStrat -> Forest a -> Idx -> a -> Forest a
-- on right level, and we'll assume correct subtree.
sub args forest (Idx (0::Int) n) a = 
  snd $ mapAccumL f 0 forest
  where
  f i node | i == n = ( i+1, Node a $ subf (subForest node) )
           | True   = ( i+1, node )
  subf frst = case args of
                ReplaceSubs -> map (fmap $ \_ -> a) frst -- Replace subforests
                _           -> [] -- Chop the subforest
sub args forest idx a = 
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
    newRootLabel = case args of 
                     Path -> a 
                     _    -> rootLabel t
    newTree = Node newRootLabel (sub args (subForest t) (Idx l n) a)

---------------------------------------------------------------------------------
-- Operations on SubTypes.
---------------------------------------------------------------------------------

-- | Make a substitution Forest (all proper children).  Initially we don't
-- replace anything.
mkSubstForest :: SubTypes a => a -> Forest Subst
mkSubstForest a = map tMap (forestRep a)
  where tMap t = fmap (\_ -> Keep) t

---------------------------------------------------------------------------------

-- | Replace a value at index idx generically in a Tree/Forest generically.
replaceAtIdx :: (SubTypes a, Data b)
             => a     -- ^ Parent value
             -> Idx   -- ^ Index of hole to replace
             -> b     -- ^ Value to replace with
             -> Maybe a
replaceAtIdx m idx = replaceChild m (subForestPath (mkSubstForest m) idx Subst)

---------------------------------------------------------------------------------

-- | Generically replace child i in m with value s.  A total function: returns
-- Nothing if you try to replace a child with an ill-typed child s.  (Returns
-- Just (the original data) if your index is out of bounds).
replaceChild :: (Data a, Data b) => a -> Forest Subst -> b -> Maybe a
replaceChild d idx s = 
  case runState (gmapM f d) (Left (), idx) of
    (d', (Left  _, _)) -> Just d'
    (_ , (Right _, _)) -> Nothing

  where
  f :: forall b. Data b 
    => b -> State (Either () (), Forest Subst) b
  f x = do 
    (lr, j)  <- get 
    case j :: Forest Subst of
      []                              -> return x
      ((Node Subst ls):rst) | null ls -> case cast s of
                                           Just x' -> do put (lr, rst)
                                                         return x'
                                           Nothing -> do put (Right (), rst)
                                                         return x
                            | True    -> case replaceChild x ls s of
                                           Just x' -> do put (lr, rst)
                                                         return x'
                                           Nothing -> do put (Right (), rst)
                                                         return x
      ((Node Keep _):rst)             -> do put (lr, rst)
                                            return x

---------------------------------------------------------------------------------
