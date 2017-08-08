{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Test.SmartCheck.Types
  ( SubT(..)
  , subT
  , Result(..)
  , SubTypes(..)
  , Idx(..)
  , Subst(..)
  , Replace(..)
  , errorMsg
  -- ** For constructing new instances of `SubTypes`
  , gst
  , grc
  , gtc
  , gsf
--  , gsz
  ) where

import GHC.Generics
import Data.Tree
import Data.Typeable
import Control.Monad (ap)

-- For instances
import Data.Word
import Data.Int
import Data.Ratio
import Data.Complex

import qualified Test.QuickCheck as Q

-------------------------------------------------------------------------------

-- | Nominally, a list for value generalization indexes and existential
-- generalization.
data Replace a = Replace { unVals :: [a], unConstrs :: [a] }
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Result type
--------------------------------------------------------------------------------

-- | Possible results of iterateArb.
data Result a =
    BaseType      -- ^ Base type. Won't analyze.
  | FailedPreCond -- ^ Couldn't satisfy the precondition of a QuickCheck
                  --   property
  | FailedProp    -- ^ Failed the property---either we expect failure and it
                  --   passes or we expect to pass it and we fail.
  | Result a      -- ^ Satisfied it, with the satisfying value.
  deriving (Show, Read, Eq)

instance Functor Result where
  fmap _ BaseType      = BaseType
  fmap _ FailedPreCond = FailedPreCond
  fmap _ FailedProp    = FailedProp
  fmap f (Result a)    = Result (f a)

instance Monad Result where
  return a            = Result a
  BaseType      >>= _ = BaseType
  FailedPreCond >>= _ = FailedPreCond
  FailedProp    >>= _ = FailedProp
  Result a      >>= f = f a

instance Applicative Result where
 pure  = return
 (<*>) = ap

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

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

-- | Sort in order of depth first then left to right.
instance Ord Idx where
  compare (Idx l0 c0) (Idx l1 c1) | l0 < l1 = LT
                                  | l0 > l1 = GT
                                  | c0 < c1 = LT
                                  | c0 > c1 = GT
                                  | True    = EQ

-------------------------------------------------------------------------------
-- User-defined subtypes of data
-------------------------------------------------------------------------------

data SubT = forall a. (Q.Arbitrary a, SubTypes a)
          => SubT { unSubT :: a }

subT :: (Q.Arbitrary a, SubTypes a) => a -> SubT
subT = SubT

-- Would require SubT to derive Eq.
-- instance Eq SubT where
--   SubT a == SubT b = cast a == Just b

instance Show SubT where
  show (SubT t) = show t

-- | This class covers algebraic datatypes that can be transformed into Trees.
-- subTypes is the main method, placing values into trees.
--
-- for a datatype with constructors A and C,
--
-- > subTypes (A (C 0) 1)
-- > [Node {rootLabel = C 0, subForest = []}]
--
class (Q.Arbitrary a, Show a, Typeable a) => SubTypes a where
  -----------------------------------------------------------
  -- | Turns algebraic data into a forest representation.
  subTypes :: a -> Forest SubT
  default subTypes :: (Generic a, GST (Rep a))
                   => a -> Forest SubT
  subTypes = gst . from
  -----------------------------------------------------------
  -- | Base types (e.g., Int, Char) aren't analyzed.
  baseType :: a -> Bool
  baseType _ = False
  -----------------------------------------------------------
  -- | Generically replace child i in m with value s.  A total function: returns
  -- Nothing if you try to replace a child with an ill-typed child s.  (Returns
  -- Just (the original data) if your index is out of bounds).
  replaceChild :: Typeable b => a -> Forest Subst -> b -> Maybe a
  default replaceChild :: (Generic a, GST (Rep a), Typeable b)
                       => a -> Forest Subst -> b -> Maybe a
  replaceChild a forest b = fmap to $ grc (from a) forest b
  -----------------------------------------------------------
  -- | Get the string representation of the constructor.
  toConstr :: a -> String
  default toConstr :: (Generic a, GST (Rep a)) => a -> String
  toConstr = gtc . from
  -----------------------------------------------------------
  -- | showForest generically shows a value while preserving its structure (in a
  -- Tree).  Always returns either a singleton list containing the tree (a
  -- degenerate forest) or an empty list for baseTypes.  An invariant is that
  -- the shape of the tree produced by showForest is the same as the one
  -- produced by subTypes.
  showForest :: a -> Forest String
  default showForest :: (Generic a, GST (Rep a))
                     => a -> Forest String
  showForest = gsf . from
  -----------------------------------------------------------


-------------------------------------------------------------------------------
-- Generic representation
-------------------------------------------------------------------------------

class GST f where
  -- Names are abbreviations of the corresponding method names above.
  gst :: f a -> Forest SubT
  grc :: Typeable b => f a -> Forest Subst -> b -> Maybe (f a)
  gtc :: f a -> String
  gsf :: f a -> Forest String
  gsz :: f a -> Int

instance GST U1 where
  gst U1 = []
  grc _ _ _ = Nothing
  gtc U1 = ""
  gsf U1 = []
  gsz U1 = 0

instance (GST a, GST b) => GST (a :*: b) where
  gst (a :*: b) = gst a ++ gst b

  grc (a :*: b) forest c =
    case forest of
      []    -> Just (a :*: b)
      ls    -> do let (x,y) = splitAt (gsz a) ls
                  left  <- grc a x c
                  right <- grc b y c
                  return $ left :*: right

  gtc (a :*: b) = gtc a ++ gtc b
  gsf (a :*: b) = gsf a ++ gsf b
  gsz (a :*: b) = gsz a + gsz b

instance (GST a, GST b) => GST (a :+: b) where
  gst (L1 a) = gst a
  gst (R1 b) = gst b

  grc (L1 a) forest c = grc a forest c >>= return . L1
  grc (R1 a) forest c = grc a forest c >>= return . R1

  gtc (L1 a) = gtc a
  gtc (R1 a) = gtc a

  gsf (L1 a) = gsf a
  gsf (R1 a) = gsf a

  gsz (L1 a) = gsz a
  gsz (R1 a) = gsz a

-- Constructor meta-information
instance (Constructor c, GST a) => GST (M1 C c a) where
  gst (M1 a) = gst a
  grc (M1 a) forest c = grc a forest c >>= return . M1
  gtc = conName

  gsf m@(M1 a) = [ Node (conName m) (gsf a) ]

  gsz (M1 a) = gsz a

-- All the other meta-information (selector, module, etc.)
instance GST a => GST (M1 i k a) where
  gst (M1 a) = gst a
  grc (M1 a) forest c = grc a forest c >>= return . M1
  gtc (M1 a) = gtc a
  gsf (M1 a) = gsf a
  gsz (M1 a) = gsz a

instance (Show a, Q.Arbitrary a, SubTypes a, Typeable a) => GST (K1 i a) where
  gst (K1 a) = if baseType a
                 then [ Node (subT a) [] ]
                 else [ Node (subT a) (subTypes a) ]

  grc (K1 a) forest c =
    case forest of
      []                  -> Just (K1 a)
      (Node Keep  _  : _) -> Just (K1 a)
      (Node Subst [] : _) -> fmap K1 (cast c)
      (Node Subst ls : _) -> replaceChild a ls c >>= return . K1

  gtc _ = ""

  gsf (K1 a) = if baseType a then [ Node (show a) [] ] else showForest a

  gsz _ = 1

-------------------------------------------------------------------------------
-- We cover the instances supported by QuickCheck:
-- http://hackage.haskell.org/packages/archive/QuickCheck/2.4.2/doc/html/Test-QuickCheck-Arbitrary.html

instance SubTypes Bool where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Char where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Double where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Float where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Int where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Integer where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Int8    where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Int16   where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Int32   where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Int64   where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Word    where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Word8   where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Word16  where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Word32  where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes Word64  where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance SubTypes ()      where baseType _    = True
instance ( Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes [a]
instance (Integral a, Q.Arbitrary a, SubTypes a, Typeable a)
  => SubTypes (Ratio a)   where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance (RealFloat a, Q.Arbitrary a, SubTypes a, Typeable a)
  => SubTypes (Complex a) where
  subTypes _    = []
  baseType _    = True
  replaceChild  = replaceChild'
  toConstr      = toConstr'
  showForest    = showForest'
instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes (Maybe a)
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b)
         => SubTypes (Either a b)
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b)
         => SubTypes (a, b)
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b
         , Q.Arbitrary c, SubTypes c, Typeable c)
         => SubTypes (a, b, c)
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b
         , Q.Arbitrary c, SubTypes c, Typeable c
         , Q.Arbitrary d, SubTypes d, Typeable d)
         => SubTypes (a, b, c, d)
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b
         , Q.Arbitrary c, SubTypes c, Typeable c
         , Q.Arbitrary d, SubTypes d, Typeable d
         , Q.Arbitrary e, SubTypes e, Typeable e)
         => SubTypes (a, b, c, d, e)

-------------------------------------------------------------------------------
-- Helpers

-- These should never be directly called.  We provide compatible instances anyway.
toConstr' :: Show a => a -> String
toConstr' = show

replaceChild' :: (Typeable a, Typeable b)
              => a -> Forest Subst -> b -> Maybe a
replaceChild' a []                 _ = Just a
replaceChild' a (Node Keep  _ : _) _ = Just a
replaceChild' _ (Node Subst _ : _) b = cast b

showForest' :: Show a => a -> Forest String
showForest' _ = []

-------------------------------------------------------------------------------

errorMsg :: String -> a
errorMsg loc = error $ "SmartCheck error: unexpected error in " ++ loc
    ++ ".  Please file a bug report at "
    ++ "<https://github.com/leepike/SmartCheck/issues>."

-------------------------------------------------------------------------------
