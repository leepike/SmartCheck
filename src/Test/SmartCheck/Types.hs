{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverlappingInstances #-}

module Test.SmartCheck.Types
  ( SubT(..)
  , subT
  , SubTypes(..) 
  , Idx(..)
  , Subst(..)
  , ScArgs(..)
  , Format(..)
  , scStdArgs
  )
    where

import GHC.Generics
import Data.Tree
import Data.Typeable

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

data SubT = forall a. (Q.Arbitrary a, Show a, Typeable a) 
          => SubT { unSubT :: a }

subT :: (Q.Arbitrary a, Show a, Typeable a) => a -> SubT
subT = SubT

-- instance Eq SubT where
--   SubT a == SubT b = cast a == Just b

instance Show SubT where
  show (SubT t) = show t

-- | This class covers algebraic datatypes that can be transformed into Trees.
-- subTypes is the main method, placing values into trees.  For types can can't
-- be put into a *structural* order (e.g., Int), we don't want SmartCheck to
-- touch them, so that aren't placed in the tree (the baseType method tells
-- subTypes which types have this property).  allSubTypes is a method that puts
-- everything into the tree, and this is used only to render the value.
--
-- for a datatype with constructors A and C, 
-- 
-- > subTypes (A (C 0) 1)
-- > [Node {rootLabel = C 0, subForest = []}]
--
-- > allSubTypes (A (C 0) 1)
-- > [Node {rootLabel = C 0, subForest = []},Node {rootLabel = 1, subForest = []}]
class Show a => SubTypes a where
  -----------------------------------------------------------
  subTypes :: a -> Forest SubT
  default subTypes :: (Generic a, GST (Rep a)) 
                   => a -> Forest SubT
  subTypes = gst . from
  -----------------------------------------------------------
  allSubTypes :: a -> Forest SubT
  default allSubTypes :: (Generic a, GST (Rep a)) 
                      => a -> Forest SubT
  allSubTypes = gat . from
  -----------------------------------------------------------
  baseType :: a -> Bool
  default baseType :: (Generic a, GST (Rep a)) => a -> Bool
  baseType _ = False
  -----------------------------------------------------------
  -- | Generically replace child i in m with value s.  A total function: returns
  -- Nothing if you try to replace a child with an ill-typed child s.  (Returns
  -- Just (the original data) if your index is out of bounds).
  replaceChild :: Typeable b => a -> Forest Subst -> b -> Maybe a
  default replaceChild :: (Generic a, GST (Rep a), Typeable b) 
                       => a -> Forest Subst -> b -> Maybe a
  replaceChild a forest b = fmap to $ grp (from a) forest b
  -----------------------------------------------------------
  -- Grab the contructor and any baseType values that follow.
  toConstrAndBase :: a -> String
  default toConstrAndBase :: (Generic a, GST (Rep a)) => a -> String
  toConstrAndBase = gcb . from
  -----------------------------------------------------------

---------------------------------------------------------------------------------
-- Generic representation
---------------------------------------------------------------------------------

class GST f where
  gst :: f a -> Forest SubT
  gat :: f a -> Forest SubT
  grp :: Typeable b => f a -> Forest Subst -> b -> Maybe (f a)
  gcb :: f a -> String

instance GST U1 where
  gst U1 = []
  gat U1 = []
  grp _ _ _ = Nothing
  gcb U1 = ""

instance (GST a, GST b) => GST (a :*: b) where
  gst (a :*: b) = gst a ++ gst b
  gat (a :*: b) = gat a ++ gat b

  grp (a :*: b) forest c 
    -- If the 1st element is a baseType, we skip it.
    | null (gst a) = grp b forest c >>= \x -> return (a :*: x)
    | otherwise       = 
        case forest of
          []                       -> Just (a :*: b)
          (n@(Node Subst _) : _)   -> do left  <- grp a [n] c 
                                         return $ left :*: b
          (Node Keep _ : rst)      -> do right <- grp b rst c
                                         return $ a :*: right

  gcb (a :*: b) = if null (gst a) 
                    then if null (gst b)
                           then gcb a ++ ' ' : gcb b
                           else gcb a
                    else if null (gst b) then gcb b
                           else ""

instance (GST a, GST b) => GST (a :+: b) where
  gst (L1 a) = gst a
  gst (R1 b) = gst b

  gat (L1 a) = gat a
  gat (R1 b) = gat b

  grp (L1 a) forest c = grp a forest c >>= return . L1
  grp (R1 a) forest c = grp a forest c >>= return . R1

  gcb (L1 a) = gcb a
  gcb (R1 a) = gcb a

instance (Constructor c, GST a) => GST (M1 C c a) where
  gst (M1 a) = gst a
  gat (M1 a) = gat a
  grp (M1 a) forest c = grp a forest c >>= return . M1
  gcb m@(M1 a) = conName m ++ ' ' : gcb a

instance GST a => GST (M1 i k a) where
  gst (M1 a) = gst a
  gat (M1 a) = gat a
  grp (M1 a) forest c = grp a forest c >>= return . M1
  gcb (M1 a) = gcb a

instance (Show a, Q.Arbitrary a, SubTypes a, Typeable a) => GST (K1 i a) where
  gst (K1 a) = if baseType a then []
                 else [ Node (subT a) (subTypes a) ]

  gat (K1 a) = [ Node (subT a) (subTypes a) ]

  grp (K1 a) forest c = 
    case forest of
      []                  -> Just (K1 a)
      (Node Keep  _  : _) -> Just (K1 a)
      (Node Subst [] : _) -> fmap K1 (cast c)
      (Node Subst ls : _) -> replaceChild a ls c >>= return . K1

  gcb (K1 a) = if baseType a then show a
                 else ""

---------------------------------------------------------------------------------

instance SubTypes Bool    where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []

instance SubTypes Int     where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []

instance SubTypes Integer where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild a []                 _ = Just a
  replaceChild a (Node Keep  _ : _) _ = Just a
  replaceChild _ (Node Subst _ : _) b = cast b
  toConstrAndBase a = show a

instance SubTypes Char    where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []

instance SubTypes String  where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild a []                 _ = Just a
  replaceChild a (Node Keep  _ : _) _ = Just a
  replaceChild _ (Node Subst _ : _) b = cast b

instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes [a] where 
  subTypes   = concatMap subTypes
  baseType _ = False
  allSubTypes = concatMap allSubTypes

---------------------------------------------------------------------------------
