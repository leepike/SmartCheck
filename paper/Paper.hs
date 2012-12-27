{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Paper where

import Prelude hiding (fail)
import Data.Maybe (catMaybes)
import qualified Test.QuickCheck as Q

cast :: SubTypes a => a -> Maybe b
cast _ = undefined

class Arbitrary a where
--  arbitrary :: IO a

sizedArbitrary :: forall a. SubTypes a => Size -> a -> IO a
sizedArbitrary sz _ = return ((undefined sz) :: a)

--------------------------------------------------------------------------------

data Format = PrintTree | PrintString
  deriving (Eq, Read, Show)

-- SmartCheck arguments
format       :: Format    -- ^ How to show extrapolated formula
format       = undefined
qcArgs       :: Q.Args    -- ^ QuickCheck arguments
qcArgs       = undefined
qc           :: Bool      -- ^ Should we run QuickCheck?  (If not, you are
                          -- expected to pass in data to analyze.)
qc           = undefined
extrap       :: Bool      -- ^ Should we extrapolate?
extrap       = undefined
constrGen    :: Bool      -- ^ Should we try to generalize constructors?
constrGen    = undefined
scMaxSuccess :: Int       -- ^ How hard (number of rounds) to look for failures
                          -- during the extrapolation and constructor
                          -- generalization stages.
scMaxSuccess = undefined
scMaxFailure :: Int       -- ^ How hard (number of rounds) to look for failure
                          -- in the reduction stage.
scMaxFailure = undefined
scMaxSize    :: Int       -- ^ Maximum size of data to generate, in terms of the
                          -- size parameter of QuickCheck's Arbitrary instance
                          -- for your data.
scMaxSize    = undefined
scMaxDepth   :: Maybe Int -- ^ How many levels into the structure of the failed
                          -- value should we descend when reducing or
                          -- generalizing?  Nothing means we go down to base
                          -- types.
scMaxDepth   = undefined

scMinExtrap  :: Int       -- ^ Minimum number of times a property's precondition
                          -- must be passed to generalize it.
scMinExtrap  = undefined

--------------------------------------------------------------------------------

data Property

-- sized :: [a] -> Int -> [a]
-- sized = undefined

data SubVal = forall a. SubTypes a => SubVal a

type Size = Int
type Idx = Int

class Arbitrary a => SubTypes a where
  size    :: a -> Size
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
        vs <- newVals v (getSize v) scMaxFailure
        case test cex idx vs prop of
          Nothing   -> reduce' (idx+1)
          Just cex' -> reduce cex' prop

getSize :: SubVal -> Size
getSize (SubVal a) = size a

newVals :: SubVal -> Size -> Int -> IO [SubVal]
newVals (SubVal a) sz tries =
  sequence (replicate tries s)
  where
  s  =     sizedArbitrary sz a
       >>= return . SubVal

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
    vs <- newVals v (getSize v) scMaxFailure
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
        vs <- newVals v scMaxSize scMaxSuccess
        if allFail cex idx vs prop
          then extrapolate' (idx+1) (idx:idxs)
          else extrapolate' (idx+1) idxs

allFail :: SubTypes a
     => a -> Idx -> [SubVal]
     -> (a -> Property) -> Bool
allFail cex idx vs prop =
  length res >= scMinExtrap && and res
  where
  res  = catMaybes (map go vs)
  go   = fail prop . replace cex idx

maxExtrap :: Int
maxExtrap = 100

--------------------------------------------------------------------------------

-- data Tree = L | B Tree Tree

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
