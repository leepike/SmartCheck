{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- QuickCheck tests for the implementation of SmartCheck.

module Main where

import qualified Test.QuickCheck as Q

import Data.Maybe
import Data.Tree
import Control.Monad
import GHC.Generics
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Types

--------------------------------------------------------------------------------

instance (SubTypes a) => SubTypes (Tree a)

instance Q.Arbitrary a => Q.Arbitrary (Tree a) where
  arbitrary = Q.sized mkT
    where
    mkT 0 = Q.arbitrary >>= \a -> return (Node a [])
    mkT n = do len <- Q.choose (0, 4)
               a <- Q.arbitrary
               ls <- replicateM len mkT'
               return $ Node a ls
      where mkT' = mkT =<< Q.choose (0, n-1)

instance Q.Arbitrary Idx where
  arbitrary = liftM2 Idx Q.arbitrary Q.arbitrary

--------------------------------------------------------------------------------

-- Just to prevent us from getting too many Nothings from indexing too deeply.
dep :: Maybe Int
dep = Just 5

--------------------------------------------------------------------------------

-- If you take from v a sub-value v' at index i, then replace v' at index i, you
-- get v back.
prop_getReplaceIdem ::
  Tree Int -> Q.NonNegative Int -> Q.NonNegative Int -> Bool
prop_getReplaceIdem v (Q.NonNegative i) (Q.NonNegative j) =
  let x = getAtIdx v idx dep in
  case x of
    Nothing -> True
    Just st -> rep st
  where
  idx = Idx i j
  rep (SubT v') = replaceAtIdx v idx v' == Just v

--------------------------------------------------------------------------------

-- Morally, getAtIdx v idx Nothing == rootLabel $ getIdxForest (subTypes v) idx
--
-- That is, they return the same value, except getIdxForest returns the whole
-- tree.
prop_forestTreeEq :: Tree Int -> Q.Positive Int -> Q.NonNegative Int -> Bool
prop_forestTreeEq v (Q.Positive i) (Q.NonNegative j) =
  let mx = getAtIdx v idx Nothing :: Maybe SubT in
  let my = getIdxForest (subTypes v) idx :: Maybe (Tree SubT) in
  (isNothing mx && isNothing my) || go mx my == Just True
  where
  -- XXX Hack! Since SubTypes doesn't derive Eq.
  exEq (SubT x) (SubT y) = show x == show y
  idx = Idx i j
  go a b = do
   x <- a
   y <- b
   return $ exEq x (rootLabel y)

--------------------------------------------------------------------------------
-- Prop:
-- null (subTypes v) iff null (showForest v)
--------------------------------------------------------------------------------


-- Some random values.
vals :: IO ()
vals = Q.sample (Q.resize 5 Q.arbitrary :: Q.Gen (Tree Int))

main :: IO ()
main = do
  Q.quickCheck prop_getReplaceIdem
  Q.quickCheck prop_forestTreeEq

--------------------------------------------------------------------------------
