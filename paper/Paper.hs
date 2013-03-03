{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

-- | Model of the SmartCheck algorithms explained in the paper.  We leave a
-- number of functions undefined here.

-- Limit: col 50

module Paper where

import Prelude hiding (fail)
import Data.Maybe (mapMaybe)
import Data.Tree
import Control.Monad (liftM, replicateM)
import Test.QuickCheck

--------------------------------------------------------------------------------

-- Arguments

data Format = PrintTree | PrintString
  deriving (Eq, Read, Show)

-- SmartCheck arguments
data ScArgs = ScArgs {
    format       :: Format    -- ^ How to show extrapolated formula
  , qcArgs       :: Args    -- ^ QuickCheck arguments
  , qc           :: Bool      -- ^ Should we run QuickCheck?  (If not, you are
                              -- expected to pass in data to analyze.)
  , scMaxSize    :: Int       -- ^ Maximum size of data to generate, in terms of
                              -- the size parameter of QuickCheck's Arbitrary
                              -- instance for your data.
  , scMaxDepth   :: Maybe Int -- ^ How many levels into the structure of the
                              -- failed value should we descend when reducing or
                              -- generalizing?  Nothing means we go down to base
                              -- types.
  , scMaxReduce  :: Int       -- ^ How hard (number of rounds) to look for
                              -- failure in the reduction stage.
  , runForall    :: Bool      -- ^ Should we extrapolate?
  , scMaxForall  :: Int       -- ^ How hard (number of rounds) to look for
                              -- failures during the extrapolation stage.
  , scMinForall  :: Int       -- ^ Minimum number of times a property's
                              -- precondition must be passed to generalize it.
  , runExists    :: Bool      -- ^ Should we try to generalize constructors?
  , scMaxExists  :: Int       -- ^ How hard (number of rounds) to look for
                              -- failing values with each constructor.  For
                              -- "wide" sum types, this value should be
                              -- increased.
}
--------------------------------------------------------------------------------

-- Types
data SubVal = forall a. SubTypes a => SubVal a

type Size = Int
type Idx = Int

class Arbitrary a => SubTypes a where
  size     :: a -> Size
  index    :: a -> Idx -> Maybe SubVal
  replace  :: a -> Idx -> SubVal -> a
  subVals  :: a -> Tree SubVal
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

sizedArbitrary ::
  forall a . SubTypes a => Size -> a -> IO a
sizedArbitrary sz _ = return (undefined sz :: a)

subTree :: SubTypes a => a -> Idx -> Idx -> Bool
subTree _ _ _ = undefined

--------------------------------------------------------------------------------

getSize :: SubVal -> Size
getSize (SubVal a) = size a

newVals :: Size -> Int -> SubVal -> IO [SubVal]
newVals sz tries (SubVal a) =
  replicateM tries s where
  s  = liftM SubVal (sizedArbitrary sz a)

reduce :: SubTypes a
  => ScArgs -> (a -> Property) -> a -> IO a
reduce args prop cex = reduce' 1
  where
  reduce' idx
    | Just v <- index cex idx
    = do vs <- newVals (getSize v)
                 (scMaxReduce args) v
         maybe (reduce' (idx+1)) (reduce args prop)
               (test cex idx vs prop)
    | otherwise = return cex

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

reduceOpt :: forall a . SubTypes a
  => ScArgs -> (a -> Property) -> a -> IO a
reduceOpt args prop cex = reduce' 1
  where
  reduce' idx
    | Just v <- index cex idx
    = maybe (test' v idx) (reduceOpt args prop)
            (testHole v)
    | otherwise = return cex

  test' v idx = do
    vs <- newVals (getSize v) (scMaxReduce args) v
    maybe (reduce' (idx+1)) (reduceOpt args prop)
          (test cex idx vs prop)

  testHole (SubVal a) = do
    a' <- cast a :: Maybe a
    if pass prop a' then Nothing else Just a'

--------------------------------------------------------------------------------

subTrees :: SubTypes a => a -> Idx -> [Idx] -> Bool
subTrees cex idx = any (subTree cex idx)

extrapolate :: SubTypes a
  => ScArgs -> a -> (a -> Property) -> IO [Idx]
extrapolate args cex prop = extrapolate' 1 []
  where
  extrapolate' idx idxs
    | subTrees cex idx idxs
    = extrapolate' (idx+1) idxs
    | Just v <- index cex idx = mkNewVals v
    | otherwise = return idxs
    where
    mkNewVals v = do
      vs <- newVals (scMaxSize args)
                    (scMaxForall args) v
      extrapolate' (idx+1)
        (if allFail args cex idx vs prop
           then idx:idxs else idxs)

allFail :: SubTypes a => ScArgs -> a -> Idx
  -> [SubVal] -> (a -> Property) -> Bool
allFail args cex idx vs prop =
  length res >= scMinForall args && and res
  where
  res  = mapMaybe go vs
  go   = fail prop . replace cex idx

--------------------------------------------------------------------------------

subConstr :: SubVal -> String
subConstr (SubVal a) = constr a

subConstrs :: SubVal -> [String]
subConstrs (SubVal a) = constrs a

sumTest :: SubTypes a => ScArgs -> a
  -> (a -> Property) -> [Idx] -> IO [Idx]
sumTest args cex prop exIdxs = sumTest' 1 []
  where
  sumTest' idx idxs
    | subTrees cex idx (exIdxs ++ idxs)
    = sumTest' (idx+1) idxs
    | Just v <- index cex idx = fromSumTest v
    | otherwise = return idxs
    where
    fromSumTest v = do
      vs <- newVals (scMaxSize args)
              (scMaxExists args) v
      sumTest' (idx+1)
        (if constrFail cex idx vs prop
           (subConstr v) (subConstrs v)
           then idx:idxs else idxs)

constrFail :: SubTypes a => a -> Idx -> [SubVal]
  -> (a -> Property) -> String -> [String] -> Bool
constrFail cex idx vs prop con allCons =
  constrFail' [con] vs
  where
  constrFail' cons vs'
    | length cons == length allCons = True
    | null vs'                      = False
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

matchesShapes :: SubTypes a
  => a -> [(a,[Idx])] -> Bool
matchesShapes d = any (matchesShape d)

-- | At each index that we generalize (either value generalization or
-- constructor generalization), we replace that value from b into a.  At this
-- point, we check for constructor equality between the two values, decending
-- their structures.
matchesShape :: SubTypes a
  => a -> (a, [Idx]) -> Bool
matchesShape a (b, idxs)
  | constr a /= constr b = False
  | Just a' <- aRepl
  = let x = subForest (subVals a') in
    let y = subForest (subVals b)  in
    all foldEqConstrs (zip x y)
  | otherwise = False
  where
  updateA idx d =
    fmap (replace d idx) (index b idx)
  aRepl = foldl go (Just a) idxs where
    go ma idx | Just x <- ma = updateA idx x
              | otherwise    = Nothing
  foldEqConstrs ( Node (SubVal l0) sts0
                , Node (SubVal l1) sts1 )
    -- Don't need a baseType test, since they don't ever appear in subTypes.
    | constr l0 == constr l1 =
      all foldEqConstrs (zip sts0 sts1)
    | otherwise              = False
