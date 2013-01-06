{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

-- | Model of the SmartCheck algorithms explained in the paper.  We leave a
-- number of functions undefined here.

-- Limit: col 46

module Paper where

import Prelude hiding (fail)
import Data.Maybe (mapMaybe)
import Control.Monad (liftM, replicateM)
import Test.QuickCheck

--------------------------------------------------------------------------------

-- Arguments

data Format = PrintTree | PrintString
  deriving (Eq, Read, Show)

-- SmartCheck arguments
format       :: Format    -- ^ How to show extrapolated formula
format       = undefined
qcArgs       :: Args    -- ^ QuickCheck arguments
qcArgs       = undefined
qc           :: Bool      -- ^ Should we run QuickCheck?  (If not, you are
                          -- expected to pass in data to analyze.)
qc           = undefined
scMaxSize    :: Int       -- ^ Maximum size of data to generate, in terms of the
                          -- size parameter of QuickCheck's Arbitrary instance
                          -- for your data.
scMaxSize    = undefined
scMaxDepth   :: Maybe Int -- ^ How many levels into the structure of the failed
                          -- value should we descend when reducing or
                          -- generalizing?  Nothing means we go down to base
                          -- types.
scMaxDepth   = undefined
scMaxReduce  :: Int       -- ^ How hard (number of rounds) to look for failure
                          -- in the reduction stage.
scMaxReduce  = undefined
extrap       :: Bool      -- ^ Should we extrapolate?
extrap       = undefined
scMaxExtrap  :: Int       -- ^ How hard (number of rounds) to look for failures
                          -- during the extrapolation and constructor
                          -- generalization stages.
scMaxExtrap  = undefined
scMinExtrap  :: Int       -- ^ Minimum number of times a property's precondition
                          -- must be passed to generalize it.
scMinExtrap  = undefined
constrGen    :: Bool      -- ^ Should we try to generalize constructors?
constrGen    = undefined
scConstrMax  :: Int       -- ^ How hard (number of rounds) to look for failing
                          -- values with each constructor.  For "wide" sum
                          -- types, this value should be increased.
scConstrMax  = undefined

--------------------------------------------------------------------------------

-- Types

-- data Property

data SubVal = forall a. SubTypes a => SubVal a

type Size = Int
type Idx = Int

class Arbitrary a => SubTypes a where
  size     :: a -> Size
  index    :: a -> Idx -> Maybe SubVal
  replace  :: a -> Idx -> SubVal -> a
  constr   :: a -> String
  constrs  :: a -> [String]
  baseType :: a -> Bool

--------------------------------------------------------------------------------

-- Undefined

pass :: (a -> Property) -> a -> Bool
pass _ _ = True

-- Failed (Just False), passed (Just True), or failed precondition (Nothing).
fail :: (a -> Property) -> a -> Maybe Bool
fail _ _ = Just True

cast :: SubTypes a => a -> Maybe b
cast _ = undefined

sizedArbitrary :: forall a. SubTypes a => Size -> a -> IO a
sizedArbitrary sz _ = return (undefined sz :: a)

subTree :: SubTypes a => a -> Idx -> Idx -> Bool
subTree _ _ _ = undefined

--------------------------------------------------------------------------------

getSize :: SubVal -> Size
getSize (SubVal a) = size a

newVals :: SubVal -> Size
        -> Int -> IO [SubVal]
newVals (SubVal a) sz tries =
  replicateM tries s
  where
  s  = liftM SubVal (sizedArbitrary sz a)

reduce :: SubTypes a
  => a -> (a -> Property) -> IO a
reduce cex prop = reduce' 1
  where
  reduce' idx =
    case index cex idx of
      Nothing -> return cex
      Just v  -> do
        vs <- newVals v (getSize v)
                scMaxReduce
        case test cex idx vs prop of
          Nothing   -> reduce' (idx+1)
          Just cex' -> reduce cex' prop

test :: SubTypes a => a -> Idx -> [SubVal]
     -> (a -> Property) -> Maybe a
test cex idx vs prop = go vs
  where
  go []      = Nothing
  go (v:vs') =
    let cex' = replace cex idx v in
    if pass prop cex' then go vs'
      else Just cex'

--------------------------------------------------------------------------------

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
    vs <- newVals v (getSize v) scMaxReduce
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

subTrees :: SubTypes a
  => a -> Idx -> [Idx] -> Bool
subTrees cex idx = any (subTree cex idx)

extrapolate :: SubTypes a
  => a -> (a -> Property) -> IO [Idx]
extrapolate cex prop = extrapolate' 1 []
  where
  extrapolate' idx idxs
    | subTrees cex idx idxs
    = extrapolate' (idx+1) idxs
    | otherwise
    = case index cex idx of
        Nothing -> return idxs
        Just v  -> do
          vs <- newVals v scMaxSize scMaxExtrap
          extrapolate' (idx+1)
            (if allFail cex idx vs prop
               then idx:idxs else idxs)

allFail :: SubTypes a
  => a -> Idx -> [SubVal]
  -> (a -> Property) -> Bool
allFail cex idx vs prop =
  length res >= scMinExtrap && and res
  where
  res  = mapMaybe go vs
  go   = fail prop . replace cex idx

--------------------------------------------------------------------------------

subConstr :: SubVal -> String
subConstr (SubVal a) = constr a

subConstrs :: SubVal -> [String]
subConstrs (SubVal a) = constrs a

sumTest :: SubTypes a
  => a -> (a -> Property) -> [Idx] -> IO [Idx]
sumTest cex prop exIdxs = sumTest' 1 []
  where
  sumTest' idx idxs
    | subTrees cex idx (exIdxs ++ idxs)
    = sumTest' (idx+1) idxs
    | otherwise
    = case index cex idx of
        Nothing -> return idxs
        Just v  -> do
          vs <- newVals v scMaxSize
                  scConstrMax
          sumTest' (idx+1)
            (if constrFail cex idx vs prop
                  (subConstr v) (subConstrs v)
               then idx:idxs else idxs)

constrFail :: SubTypes a
  => a -> Idx -> [SubVal] -> (a -> Property)
  -> String -> [String] -> Bool
constrFail cex idx vs prop con allCons =
  constrFail' [con] vs
  where
  constrFail' cons vs'
    | length cons == length allCons
    = True
    | null vs'
    = False
    | go v == Just True
    = constrFail' (c:cons) (tail vs')
    | otherwise
    = constrFail' cons (tail vs')

    where
    v  = head vs'
    c  = subConstr v
    go = fail prop' . replace cex idx
    prop' a = c `notElem` cons ==> prop a

--------------------------------------------------------------------------------

-- matchesShapes :: SubTypes a => ScArgs -> a -> [a] -> [Idx] -> Bool
-- matchesShapes args d ds idxs = foldl' f False ds
--   where
--   f True _   = True
--   f False d' = matchesShape args d d' idxs

matchesShape :: SubTypes a => a -> a -> [Idx] -> Bool
matchesShape a b idxs = match (a, b) && repIdxs
  where

  -- Says that every value v at index idx from a can be put into b at the same
  -- idx and its type-correct.
  --
  -- If we can do that, then we take the resulting value (b with a's idxes in
  -- it) and a, and get all the immediate children, and we test if they have the
  -- same constructors (or are base types).
  --
  -- XXX why are we only taking the immediate subchildren?
  repIdxs = case foldl go (Just b) idxs of
              Nothing -> False
              Just b' -> all match $ zip (nextLevel a) (nextLevel b')

  go mb idx = do
    b' <- mb
    v  <- index a idx
    replace b' idx v

  nextLevel x  = map rootLabel (subTypes x)
  match (x, y) = baseType x || toConstr x == toConstr y
