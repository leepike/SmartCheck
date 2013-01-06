{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests where

import qualified Test.QuickCheck as Q

import Data.Tree
import Control.Monad
import GHC.Generics
import Test.SmartCheck.DataToTree
import Test.SmartCheck.Types

--------------------------------------------------------------------------------

deriving instance Generic a =>  Generic (Tree a)
instance (SubTypes a, Generic a) => SubTypes (Tree a)

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

d :: Maybe Int
d = Just 5

prop_getReplaceIdem :: Tree Int -> Q.NonNegative Int -> Q.NonNegative Int -> Bool
prop_getReplaceIdem t (Q.NonNegative i) (Q.NonNegative j) =
  let x = getAtIdx t idx d in
  case x of
    Nothing -> True
    Just st -> rep st
  where
  idx = Idx i j
  rep (SubT t') = replaceAtIdx t idx t' == Just t

-- prop_forestTreeEq :: Tree Int -> Q.Positive Int -> Q.NonNegative Int -> Bool
-- prop_forestTreeEq t (Q.Positive i) (Q.NonNegative j) =
--   case getAtIdx t idx d of
--     Nothing -> isNothing idf
--     Just t' -> extract t'
--   where
--   idf = getIdxForest [t] idx
--   extract (SubT t') = case cast t' :: Maybe (Tree Int) of
--                         Nothing -> False
--                         Just t_ -> Just t_ == idf
--   idx = Idx i j

vals :: IO ()
vals = Q.sample (Q.resize 5 Q.arbitrary :: Q.Gen (Tree Int))

main :: IO ()
main = do
  Q.quickCheck prop_getReplaceIdem
--  Q.quickCheck prop_forestTreeEq
