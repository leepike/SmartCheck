{-# LANGUAGE CPP #-}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Copied from QuickCheck2's examples.

module Main where

--------------------------------------------------------------------------
-- imports

#if defined(qc) || defined(qcGen) || defined(smart)
import Test
import System.Environment
#endif
import Test.SmartCheck

import Test.QuickCheck
import Test.QuickCheck.Poly

import Data.List
  ( sort
  )
import Data.Typeable

import GHC.Generics

import qualified Test.SmartCheck as SC

--------------------------------------------------------------------------
-- SmartCheck Testing.  Comment out shrink instance if you want to be more
-- impressed. :)
--------------------------------------------------------------------------

deriving instance Typeable OrdA
deriving instance Generic OrdA

instance Read OrdA where
  readsPrec i = \s ->
    let rd = readsPrec i s :: [(Integer,String)] in
    let go (i',s') = (OrdA i', s') in
    map go rd

heapProgramTest :: IO ()
heapProgramTest = SC.smartCheck SC.scStdArgs prop_ToSortedList

instance SC.SubTypes OrdA
instance (SC.SubTypes a, Ord a, Arbitrary a, Generic a)
         => SC.SubTypes (Heap a)
instance (SC.SubTypes a, Arbitrary a, Generic a)
         => SC.SubTypes (HeapP a)
instance (SC.SubTypes a, Ord a, Arbitrary a, Generic a)
         => SC.SubTypes (HeapPP a)

instance (Ord a, Arbitrary a, Typeable a) => Arbitrary (Heap a) where
  arbitrary = do p <- arbitrary :: Gen (HeapP a)
                 return $ heap p

--------------------------------------------------------------------------
-- skew heaps
-- Smallest values on top.

data Heap a
  = Node a (Heap a) (Heap a)
  | Nil
 deriving ( Eq, Ord, Show, Read, Typeable, Generic )

empty :: Heap a
empty = Nil

isEmpty :: Heap a -> Bool
isEmpty Nil = True
isEmpty _   = False

unit :: a -> Heap a
unit x = Node x empty empty

size :: Heap a -> Int
size Nil            = 0
size (Node _ h1 h2) = 1 + size h1 + size h2

insert :: Ord a => a -> Heap a -> Heap a
insert x h = unit x `merge` h

removeMin :: Ord a => Heap a -> Maybe (a, Heap a)
removeMin Nil            = Nothing
removeMin (Node x h1 h2) = Just (x, h1 `merge` h2)

merge :: Ord a => Heap a -> Heap a -> Heap a
h1  `merge` Nil = h1
Nil `merge` h2  = h2
h1@(Node x h11 h12) `merge` h2@(Node y h21 h22)
  | x <= y    = Node x (h12 `merge` h2) h11
  | otherwise = Node y (h22 `merge` h1) h21

fromList :: Ord a => [a] -> Heap a
fromList xs = merging [ unit x | x <- xs ]
 where
  merging []  = empty
  merging [h] = h
  merging hs  = merging (sweep hs)

  sweep []         = []
  sweep [h]        = [h]
  sweep (h1:h2:hs) = (h1 `merge` h2) : sweep hs

toList :: Heap a -> [a]
toList h = toList' [h]
 where
  toList' []                  = []
  toList' (Nil          : hs) = toList' hs
  toList' (Node x h1 h2 : hs) = x : toList' (h1:h2:hs)

toSortedList :: Ord a => Heap a -> [a]
toSortedList Nil            = []
toSortedList (Node x h1 h2) = x : toList (h1 `merge` h2)

--------------------------------------------------------------------------
-- heap programs

data HeapP a
  = Empty
  | Unit a
  | Insert a (HeapP a)
  | SafeRemoveMin (HeapP a)
  | Merge (HeapP a) (HeapP a)
  | FromList [a]
 deriving ( Show, Read, Typeable, Generic )

heap :: Ord a => HeapP a -> Heap a
heap Empty             = empty
heap (Unit x)          = unit x
heap (Insert x p)      = insert x (heap p)
heap (SafeRemoveMin p) = case removeMin (heap p) of
                           Nothing    -> empty -- arbitrary choice
                           Just (_,h) -> h
heap (Merge p q)       = heap p `merge` heap q
heap (FromList xs)     = fromList xs

instance (Typeable a, Arbitrary a) => Arbitrary (HeapP a) where
  arbitrary = sized arbHeapP
   where
    arbHeapP s =
      frequency
      [ (1, do return Empty)
      , (1, do x <- arbitrary
               return (Unit x))
      , (s, do x <- arbitrary
               p <- arbHeapP s1
               return (Insert x p))
      , (s, do p <- arbHeapP s1
               return (SafeRemoveMin p))
      , (s, do p <- arbHeapP s2
               q <- arbHeapP s2
               return (Merge p q))
      , (1, do xs <- arbitrary
               return (FromList xs))
      ]
     where
      s1 = s-1
      s2 = s`div`2

#ifdef qc
  shrink (Unit x)          = [ Unit x' | x' <- shrink x ]
  shrink (FromList xs)     = [ Unit x | x <- xs ]
                          ++ [ FromList xs' | xs' <- shrink xs ]
  shrink (Insert x p)      = [ p ]
                          ++ [ Insert x p' | p' <- shrink p ]
                          ++ [ Insert x' p | x' <- shrink x ]
  shrink (SafeRemoveMin p) = [ p ]
                          ++ [ SafeRemoveMin p' | p' <- shrink p ]
  shrink (Merge p q)       = [ p, q ]
                          ++ [ Merge p' q | p' <- shrink p ]
                          ++ [ Merge p q' | q' <- shrink q ]
  shrink _                 = []
#endif
#ifdef qcGen
  shrink = genericShrink
#endif

data HeapPP a = HeapPP (HeapP a) (Heap a)
 deriving ( Show, Read, Typeable, Generic )

instance (Ord a, Arbitrary a, Typeable a) => Arbitrary (HeapPP a) where
  arbitrary =
    do p <- arbitrary
       return (HeapPP p (heap p))
#ifdef qc
  shrink (HeapPP p _) =
    [ HeapPP p' (heap p') | p' <- shrink p ]
#endif
#ifdef qcGen
  shrink = genericShrink
#endif
--------------------------------------------------------------------------
-- properties

(==?) :: Heap OrdA -> [OrdA] -> Bool
h ==? xs = sort (toList h) == sort xs

prop_ToSortedList :: HeapPP OrdA -> Bool
prop_ToSortedList (HeapPP _ h) =
  h ==? xs && xs == sort xs
 where
  xs = toSortedList h

sizePP :: HeapPP a -> Int
sizePP (HeapPP h0 h1) = sizeP h0 + sizeH h1

sizeP :: HeapP a -> Int
sizeP hp = case hp of
  Empty           -> 1
  Unit _          -> 1
  Insert _ h      -> 1 + sizeP h
  SafeRemoveMin h -> 1 + sizeP h
  Merge h0 h1     -> 1 + sizeP h0 + sizeP h1
  FromList ls     -> 1 + length ls

sizeH :: Heap a -> Int
sizeH hp = case hp of
  Node a h0 h1 -> 1 + sizeH h0 + sizeH h1
  Nil          -> 1

l :: HeapP OrdA
l = FromList [OrdA 2, OrdA 1]

#if defined(qc) || defined(qcGen) || defined(smart)
main :: IO ()
main = do
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#if defined(qc) || defined(qcGen)
  test file rnds $ runQC' proxy stdArgs prop_ToSortedList (sizePP :: HeapPP OrdA -> Int)
#else
  test file rnds $ runSC scStdArgs prop_ToSortedList sizePP
#endif
#endif

