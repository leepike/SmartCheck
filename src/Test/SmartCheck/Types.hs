{-# LANGUAGE ExistentialQuantification #-} 

module Test.SmartCheck.Types
  ( SubT(..)
  , subT
  , SubTypes(..)
  , Idx(..)
  , Subst(..)
  , ScArgs(..)
  , Format(..)
  , scStdArgs
  ) where

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
-- User-defined subtypes of data
---------------------------------------------------------------------------------

data SubT = forall a. (Data a, Q.Arbitrary a, Show a) 
          => SubT { unSubT :: a }

-- instance Eq SubT where
--   SubT a == SubT b = cast a == Just b

instance Show SubT where
  show (SubT t) = show t

subT :: (Data a, Q.Arbitrary a, Show a) => a -> SubT
subT = SubT

class (Show a, Data a) => SubTypes a where
  subTypes :: a -> Forest SubT

---------------------------------------------------------------------------------
-- Indexing
---------------------------------------------------------------------------------

-- | Index into a Tree/Forest, where level is the depth from the root and column
-- is the distance d is the dth value on the same level.  Thus, all left-most
-- nodes are in column 0.  This is a "matrix view" of tree-structured data.
data Idx = Idx { level :: Int, column :: Int }
  deriving (Show, Eq, Read)

-- | Keep or substitue a value in the tree.
data Subst = Keep | Subst
  deriving (Show, Eq, Read)

---------------------------------------------------------------------------------
