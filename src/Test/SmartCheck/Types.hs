{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}


module Test.SmartCheck.Types
  ( SubT(..)
  , subT
  , SubTypes(..) -- ^ The subTypes method should only be called for rendering.
                 -- Otherwise, use forestRep, which removes "base types" like
                 -- Int, Char, etc. --- unary constructors.
  , Idx(..)
  , Subst(..)
  , ScArgs(..)
  , Format(..)
  , scStdArgs
  , forestRep
  ) where

import GHC.Generics

import Data.List
import Data.Tree
import Data.Data

import qualified Test.QuickCheck as Q

---------------------------------------------------------------------------------
-- User-defined subtypes of data
---------------------------------------------------------------------------------

data Format = PrntTree | PrntString
  deriving (Eq, Read, Show)

data ScArgs = 
  ScArgs { chatty   :: Bool   -- ^ Verbose output while running SmartCheck
         , treeShow :: Format -- ^ How to show extrapolated formula
         , qcArgs   :: Q.Args -- ^ QuickCheck arguments
         }
  deriving (Show, Read)

scStdArgs :: ScArgs
scStdArgs = ScArgs { chatty   = False
                   , treeShow = PrntTree
                   , qcArgs   = Q.stdArgs
                   }

---------------------------------------------------------------------------------
-- Indexing
---------------------------------------------------------------------------------

-- | Index into a Tree/Forest, where level is the depth from the root and column
-- is the distance d is the dth value on the same level.  Thus, all left-most
-- nodes are in column 0.  This is a "matrix view" of tree-structured data.
data Idx = Idx { level :: Int, column :: Int }
  deriving Eq

instance Show Idx where
  show (Idx l c) = foldr1 (++) ["Idx ", show l, " ", show c]

-- | Keep or substitue a value in the tree.
data Subst = Keep | Subst
  deriving (Show, Eq, Read)

---------------------------------------------------------------------------------
-- User-defined subtypes of data
---------------------------------------------------------------------------------

data SubT = forall a. (Data a, Q.Arbitrary a, Show a) 
          => SubT { unSubT :: a }

subT :: (Data a, Q.Arbitrary a, Show a) => a -> SubT
subT = SubT

-- instance Eq SubT where
--   SubT a == SubT b = cast a == Just b

instance Show SubT where
  show (SubT t) = show t

class (Show a, Data a) => SubTypes a where
  subTypes :: a -> Forest SubT

  default subTypes :: (Generic a, GST (Rep a)) => a -> Forest SubT
  subTypes = gst . from

---------------------------------------------------------------------------------
-- Generic representation
---------------------------------------------------------------------------------

class GST f where
  gst :: f a -> Forest SubT

instance GST U1 where
  gst U1 = []

instance (GST a, GST b) => GST (a :*: b) where
  gst (a :*: b) = gst a ++ gst b

instance (GST a, GST b) => GST (a :+: b) where
  gst (L1 a) = gst a
  gst (R1 b) = gst b

instance GST a => GST (M1 i c a) where
  gst (M1 x) = gst x

instance (Show a, Data a, Q.Arbitrary a, SubTypes a) => GST (K1 i a) where
  gst (K1 x) = [ Node (subT x) (subTypes x) ]
    where

---------------------------------------------------------------------------------

mkSubT :: (Data a, Q.Arbitrary a, Show a) => a -> Forest SubT
mkSubT i = [ Node (subT i) [] ]

instance SubTypes Bool   where subTypes _ = []
instance SubTypes Int    where subTypes _ = []
instance SubTypes Char   where subTypes _ = [] 

--instance SubTypes String where subTypes = concatMap mkSubT

instance (Q.Arbitrary a, SubTypes a) => SubTypes ([] a) where 
  subTypes = concatMap mkSubT

---------------------------------------------------------------------------------

-- | Remove all Nodes that contain no subforests from a Forest.
remEmptySubF :: Forest a -> Forest a
remEmptySubF ls = reverse (foldl' f [] ls)
  where
  f acc (Node _ [])      = acc
  f acc (Node y forest') = Node y (remEmptySubF forest') : acc

forestRep :: SubTypes a => a -> Forest SubT
forestRep = remEmptySubF . subTypes

---------------------------------------------------------------------------------
