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
  , errorMsg
  )
    where

import GHC.Generics
import Data.Tree
import Data.Typeable

-- For instances
import Data.Word
import Data.Int
import Data.Ratio
import Data.Complex

import qualified Test.QuickCheck as Q

---------------------------------------------------------------------------------
-- User-defined subtypes of data
---------------------------------------------------------------------------------

data Format = PrintTree | PrintString
  deriving (Eq, Read, Show)

data ScArgs = 
  ScArgs { chatty   :: Bool   -- ^ Verbose output while running SmartCheck
         , treeShow :: Format -- ^ How to show extrapolated formula
         , qcArgs   :: Q.Args -- ^ QuickCheck arguments
         }
  deriving (Show, Read)

scStdArgs :: ScArgs
scStdArgs = ScArgs { chatty   = False
                   , treeShow = PrintTree
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

data SubT = forall a. (Q.Arbitrary a, SubTypes a) 
          => SubT { unSubT :: a }

subT :: (Q.Arbitrary a, SubTypes a) => a -> SubT
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
class (Q.Arbitrary a, Show a, Typeable a) => SubTypes a where
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
  -- Grab the top contructor.
  toConstr :: a -> String
  default toConstr :: (Generic a, GST (Rep a)) => a -> String
  toConstr = gtc . from
  -----------------------------------------------------------
  -- Grab the contructor and any baseType values that follow.
  -- toConstrAndBase :: a -> String
  -- default toConstrAndBase :: (Generic a, GST (Rep a)) => a -> String
  -- toConstrAndBase = gcb . from
  -----------------------------------------------------------
  showForest :: a -> Forest String
  default showForest :: (Generic a, GST (Rep a)) 
                     => a -> Forest String
  showForest = gsf . from
  -----------------------------------------------------------

---------------------------------------------------------------------------------
-- Generic representation
---------------------------------------------------------------------------------

class GST f where
  -- Names are abbreviations of the corresponding method names above.
  gst :: f a -> Forest SubT
  gat :: f a -> Forest SubT
  grp :: Typeable b => f a -> Forest Subst -> b -> Maybe (f a)
  gtc :: f a -> String
--  gcb :: f a -> String
  gsf :: f a -> Forest String

instance GST U1 where
  gst U1 = []
  gat U1 = []
  grp _ _ _ = Nothing
  gtc U1 = ""
--  gcb U1 = ""
  gsf U1 = []

instance (GST a, GST b) => GST (a :*: b) where
  gst (a :*: b) = gst a ++ gst b
  gat (a :*: b) = gat a ++ gat b

  grp (a :*: b) forest c 
    -- If the 1st element is a baseType, we skip it.  Can't use baseTypes
    -- directly here, so we see if the tree's subforest is empty.
    | null (gst a) = grp b forest c >>= \x -> return (a :*: x)
    | otherwise       = 
        case forest of
          []                       -> Just (a :*: b)
          (n@(Node Subst _) : _)   -> do left  <- grp a [n] c 
                                         return $ left :*: b
          (Node Keep _ : rst)      -> do right <- grp b rst c
                                         return $ a :*: right

  gtc (a :*: b) = gtc a ++ gtc b

  -- If the element is a baseType, we use it.  (Can't use baseTypes directly
  -- here, so we see if the tree's subforest is empty).
  -- gcb (a :*: b) = if null (gst a) 
  --                   then if null (gst b) then addSpace (gcb a) (gcb b) 
  --                          else gcb a
  --                   else if null (gst b) then gcb b else ""
                           
  gsf (a :*: b) = gsf a ++ gsf b

instance (GST a, GST b) => GST (a :+: b) where
  gst (L1 a) = gst a
  gst (R1 b) = gst b

  gat (L1 a) = gat a
  gat (R1 b) = gat b

  grp (L1 a) forest c = grp a forest c >>= return . L1
  grp (R1 a) forest c = grp a forest c >>= return . R1

  gtc (L1 a) = gtc a
  gtc (R1 a) = gtc a

  -- gcb (L1 a) = gcb a
  -- gcb (R1 a) = gcb a

  gsf (L1 a) = gsf a
  gsf (R1 a) = gsf a

-- Constructor meta-information
instance (Constructor c, GST a) => GST (M1 C c a) where
  gst (M1 a) = gst a
  gat (M1 a) = gat a
  grp (M1 a) forest c = grp a forest c >>= return . M1
  gtc = conName 
--  gcb m@(M1 a) = addSpace (conName m) (gcb a)
  gsf m@(M1 a) = [ Node (conName m) forest ]
    where
    forest = gsf a --if null (gst a) then [] else gsf a

-- All the other meta-information (selector, module, etc.)
instance GST a => GST (M1 i k a) where
  gst (M1 a) = gst a
  gat (M1 a) = gat a
  grp (M1 a) forest c = grp a forest c >>= return . M1
  gtc (M1 a) = gtc a
--  gcb (M1 a) = gcb a
  gsf (M1 a) = gsf a

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

  gtc _ = ""
--  gcb (K1 a) = if baseType a then show a else ""
  
  gsf (K1 a) = forest
    where 
    forest = if baseType a then [ Node (show a) [] ] else showForest a

---------------------------------------------------------------------------------
-- We try to cover the instances supported by QuickCheck: http://hackage.haskell.org/packages/archive/QuickCheck/2.4.2/doc/html/Test-QuickCheck-Arbitrary.html

instance SubTypes Bool    where baseType _    = True
instance SubTypes Char    where baseType _    = True
instance SubTypes Double  where baseType _    = True
instance SubTypes Float   where baseType _    = True
instance SubTypes Int     where baseType _    = True
instance SubTypes Int8    where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Int16   where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Int32   where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Int64   where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Integer where
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Word    where
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Word8   where
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Word16  where
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Word32  where
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes Word64  where
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance SubTypes ()      where baseType _    = True
instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes [a] where 
  subTypes      = concatMap subTypes
  baseType _    = True
  allSubTypes   = concatMap allSubTypes
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance (Integral a, Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes (Ratio a) where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance (RealFloat a, Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes (Complex a) where 
  subTypes _    = []
  baseType _    = True
  allSubTypes _ = []
  replaceChild  = replaceChild'
  toConstr      = toConstr'
--  toConstrAndBase = toConstrAndBase'
  showForest    = showForest'  
instance (Q.Arbitrary a, SubTypes a, Typeable a) => SubTypes (Maybe a) where 
  baseType _ = False
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b) 
         => SubTypes (Either a b) where 
  baseType _ = False
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b) 
         => SubTypes (a, b) where 
  baseType _ = False
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b
         , Q.Arbitrary c, SubTypes c, Typeable c) 
         => SubTypes (a, b, c) where 
  baseType _ = False
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b 
         , Q.Arbitrary c, SubTypes c, Typeable c 
         , Q.Arbitrary d, SubTypes d, Typeable d) 
         => SubTypes (a, b, c, d) where 
  baseType _ = False
instance ( Q.Arbitrary a, SubTypes a, Typeable a
         , Q.Arbitrary b, SubTypes b, Typeable b 
         , Q.Arbitrary c, SubTypes c, Typeable c 
         , Q.Arbitrary d, SubTypes d, Typeable d 
         , Q.Arbitrary e, SubTypes e, Typeable e) 
         => SubTypes (a, b, c, d, e) where 
  baseType _ = False

-- We treat String specially: we don't want to rewrite them.  (This is open for
-- revision...)
instance SubTypes String  where 
  baseType _    = True

---------------------------------------------------------------------------------
-- Helpers

toConstr' :: a -> String
toConstr' _ = errorMsg "toConstr method of SubTypes Class"

-- toConstrAndBase' :: a -> String
-- toConstrAndBase' _ = errorMsg "toConstrAndBase method of SubTypes Class"

replaceChild' :: a -> Forest Subst -> b -> Maybe a
replaceChild' _ _ = errorMsg "replaceChild method of SubTypes Class"
-- replaceChild' :: (Typeable a, Typeable b) 
--               => a -> Forest Subst -> b -> Maybe a
-- replaceChild' a []                 _ = Just a
-- replaceChild' a (Node Keep  _ : _) _ = Just a
-- replaceChild' _ (Node Subst _ : _) b = cast b

showForest' :: a -> Forest String
showForest' _ = errorMsg "showForest method of SubTypes Class"

---------------------------------------------------------------------------------

-- addSpace :: String -> String -> String
-- addSpace a b = if null b then a else a ++ ' ': b

-- parens :: String -> String
-- parens a = '(' : a ++ ")"

---------------------------------------------------------------------------------

errorMsg :: String -> a
errorMsg loc = error $ "SmartCheck error: unexpected error in " ++ loc
    ++ ".  Please file a bug report at " 
    ++ "<https://github.com/leepike/SmartCheck/issues>."

---------------------------------------------------------------------------------
