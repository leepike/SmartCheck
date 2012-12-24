{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Paper where

import Prelude hiding (fail)
import Data.Maybe (catMaybes)

cast :: SubTypes a => a -> Maybe b
cast _ = undefined

class Arbitrary a where
--  arbitrary :: IO a

sizedArbitrary :: forall a. SubTypes a => a -> IO a
sizedArbitrary _ = return (undefined :: a)

unsizedArbitrary :: forall a. SubTypes a => a -> IO a
unsizedArbitrary _ = return (undefined :: a)

data Property

-- sized :: [a] -> Int -> [a]
-- sized = undefined

data SubVal = forall a. SubTypes a => SubVal a

type Idx = Int

class Arbitrary a => SubTypes a where
  size    :: a -> Idx
  index   :: a -> Idx -> Maybe SubVal
  replace :: a -> Idx -> SubVal -> a

reduce :: SubTypes a
       => a -> (a -> Property) -> IO a
reduce cex prop = reduce' 1
  where
  reduce' idx =
    case index cex idx of
      Nothing -> return cex
      Just v  -> do
        vs <-newVals v
        case test cex idx vs prop of
          Nothing   -> reduce' (idx+1)
          Just cex' -> reduce cex' prop

newVals :: SubVal -> IO [SubVal]
newVals (SubVal a) =
  sequence (replicate maxTries s)
  where
  s = sizedArbitrary a >>= return . SubVal

test :: SubTypes a
     => a -> Idx -> [SubVal]
     -> (a -> Property) -> Maybe a
test cex idx vs prop = go vs
  where
  go []      = Nothing
  go (v:vs') =
    let cex' = replace cex idx v in
    if pass prop cex' then go vs'
      else Just cex'

maxTries :: Int
maxTries = 100

pass :: (a -> Property) -> a -> Bool
pass _ _ = True

-- Failed, passed, or failed precondition.
fail :: (a -> Property) -> a -> Maybe Bool
fail _ _ = Just True

reduce0 :: forall a . SubTypes a
        => a -> (a -> Property) -> IO a
reduce0 cex prop = reduce' 1
  where
  reduce' idx =
    case index cex idx of
      Nothing -> return cex
      Just v  -> case testHole v of
                   Just v' -> reduce0 v' prop
                   Nothing -> test' v idx

  test' v idx = do
    vs <- newVals v
    case test cex idx vs prop of
      Nothing   -> reduce' (idx+1)
      Just cex' -> reduce0 cex' prop

  testHole (SubVal a) =
    case cast a :: Maybe a of
      Nothing -> Nothing
      Just a' ->
        if pass prop a' then Nothing
          else Just a'

--------------------------------------------------------------------------------

extrapolate :: SubTypes a
       => a -> (a -> Property) -> IO [Idx]
extrapolate cex prop = extrapolate' 1 []
  where
  extrapolate' idx idxs =
    case index cex idx of
      Nothing -> return idxs
      Just v  -> do
        vs <- extrapVals v
        if allFail cex idx vs prop
          then extrapolate' (idx+1) (idx:idxs)
          else extrapolate' (idx+1) idxs

extrapVals :: SubVal -> IO [SubVal]
extrapVals (SubVal a) =
  sequence (replicate maxExtrap s)
  where
  s = unsizedArbitrary a >>= return . SubVal

allFail :: SubTypes a
     => a -> Idx -> [SubVal]
     -> (a -> Property) -> Bool
allFail cex idx vs prop =
  (not (null res)) && and res
  where
  res  = catMaybes (map (fail prop) vals)
  vals = map (replace cex idx) vs

maxExtrap :: Int
maxExtrap = 100

--------------------------------------------------------------------------------

data Tree = L | B Tree Tree

-- instance SubTypes Tree where
--   size L = 1
--   size (B t0 t1) = 1 + size t0 + size 1

--   -- index t 0 = (Just . SubVal) t
--   -- index (B t0 t1) n = if size t0 >= n then

-- tree = B (B L
--              (B L L))
--           (B L L)

-- size tree = 9

-- index tree 4 = (Just . SubVal) (B L L)

-- index tree 12 = Nothing

-- replace tree 2 (SubVal L) =
--   B (B L
--         (B L L))
--      L
