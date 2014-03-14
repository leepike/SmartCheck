{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , gsz
  ) where

import GHC.Generics
import Data.Tree
import Data.Typeable

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
data Result a = FailedPreCond -- ^ Couldn't satisfy the precondition of a
                              --   QuickCheck property
              | FailedProp    -- ^ Failed the property---either we expect
                              --   failure and it passes or we expect to pass it
                              --   and we fail.
              | Result a      -- ^ Satisfied it, with the satisfying value.
  deriving (Show, Read, Eq)

instance Functor Result where
  fmap _ FailedPreCond = FailedPreCond
  fmap _ FailedProp    = FailedProp
  fmap f (Result a)    = Result (f a)

instance Monad Result where
  return a            = Result a
  FailedPreCond >>= _ = FailedPreCond
  FailedProp    >>= _ = FailedProp
  Result a      >>= f = f a

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
  subTypes :: a -> Forest SubT
  default subTypes :: (Generic a, GST (Rep a))
                   => a -> Forest SubT
  subTypes = gst . from
  -----------------------------------------------------------
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
  -- Grab the top contructor.
  toConstr :: a -> String
  default toConstr :: (Generic a, GST (Rep a)) => a -> String
  toConstr = gtc . from
  -----------------------------------------------------------
  -- | showForest generically shows a value while preserving its structure (in a
  -- Tree).  You should always end up with either a singleton list containing
  -- the tree or an empty list for baseTypes.  Also, it must be the case that
  -- for a value v,
  --
  -- null (subTypes v) iff null (showForest v)
  -- and
  -- if not . null (subTypes v), then subForest . head (showForest v)
  -- has the same structure as subTypes v.
  --
  -- We can't just return a Tree String or Maybe (Tree String).  The reason is
  -- that in generically constructing the value, we have to deal with product
  -- types.  There is no sane way to join them other than list-like
  -- concatenation (i.e., gsf (a :*: b) = gsf a ++ gsf b).
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

  gsf m@(M1 a) = [ tree ]
    where
    -- When a tree has reached a constructor with a baseType value (e.g., A 3
    -- for some constructor A), we want to show the constructor and the value,
    -- but not have a subForest.  So we check if the rest is a baseType (gst a
    -- tells us that), and if so, we show the conName, and extract (rootLabel
    -- . head) (gsf a), which is basically just showing the rest (look at gsf
    -- (K1 a) below).  Otherwise, we just want the constructor.
    tree | null (gst a) = Node root []
         | otherwise    = Node (conName m) (gsf a)
    root | null (gsf a) = conName m
         | otherwise    = conName m ++ " " ++ (rootLabel . head) (gsf a)

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
                 then []
                 else [ Node (subT a) (subTypes a) ]

  grc (K1 a) forest c =
    case forest of
      []                  -> Just (K1 a)
      (Node Keep  _  : _) -> Just (K1 a)
      (Node Subst [] : _) -> fmap K1 (cast c)
      (Node Subst ls : _) -> replaceChild a ls c >>= return . K1

  gtc _ = ""

  -- Yes, this is right.  For a baseType value v, showForest v will just yield
  -- [] using showForest'.  But to make the tree using generics, when we get
  -- down to baseTypes, we need to actually show them, returing a Forest.  We
  -- extract the value in the rootLabel above.
  gsf (K1 a) = if baseType a then [Node (show a) []] else showForest a

  gsz (K1 a) = if baseType a then 0 else 1

-------------------------------------------------------------------------------
-- We try to cover the instances supported by QuickCheck: http://hackage.haskell.org/packages/archive/QuickCheck/2.4.2/doc/html/Test-QuickCheck-Arbitrary.html

instance SubTypes Bool    where baseType _    = True
instance SubTypes Char    where baseType _    = True
instance SubTypes Double  where baseType _    = True
instance SubTypes Float   where baseType _    = True
instance SubTypes Int     where baseType _    = True
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

--instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes [a]
--   subTypes      = concatMap subTypes
--   baseType _    = True
--   replaceChild  = replaceChild'
--   toConstr      = toConstr'
-- --  toConstrAndBase = toConstrAndBase'
--  showForest    = showForest'

-- For example, this makes String a baseType automatically.
-- instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes [a] where
--   subTypes      = if baseType (undefined :: a) then \_ -> []
--                     else gst . from
--   baseType _    = baseType (undefined :: a)
--   replaceChild x forest y = if baseType (undefined :: a)
--                               then replaceChild' x forest y
--                               else fmap to $ grc (from x) forest y
--   toConstr      = if baseType (undefined :: a) then toConstr'
--                     else gtc . from
--   showForest    = if baseType (undefined :: a) then showForest'
--                     else gsf . from

-- For example, this makes String a baseType automatically.
instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes [a] where
  subTypes      = gst . from
  baseType _    = False
  replaceChild x forest y = fmap to $ grc (from x) forest y
  toConstr      = gtc . from
  showForest    = gsf . from

instance (Integral a, Q.Arbitrary a, SubTypes a, Typeable a)
  => SubTypes (Ratio a) where
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
