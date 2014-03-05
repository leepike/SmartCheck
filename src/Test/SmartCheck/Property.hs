-- | Type classes for making SmartCheck Properties (analogous to QuickCheck's
-- 'Property' class.

module Test.SmartCheck.Property where

import qualified Test.QuickCheck as Q
import Test.SmartCheck.Types

--------------------------------------------------------------------------------

-- | Type for SmartCheck properties.  Moral equivalent of QuickCheck's
-- `Property` type.
data ScProperty = Implies (Bool, Bool)
                | Simple  Bool
  deriving (Show, Read, Eq)

instance Q.Testable ScProperty where
  property (Simple prop)         = Q.property prop
  property (Implies prop)        = Q.property (toQCImp prop)
  exhaustive (Simple prop)       = Q.exhaustive prop
  exhaustive (Implies prop)      = Q.exhaustive (toQCImp prop)

-- same as ==>
infixr 0 -->
-- | Moral equivalent of QuickCheck's `==>` operator.
(-->) :: Bool -> Bool -> ScProperty
pre --> post = Implies (pre, post)

-- Helper function.
toQCImp :: (Bool, Bool) -> Q.Property
toQCImp (pre, post) = pre Q.==> post

-- | Turn a function that returns a `Bool` into a QuickCheck `Property`.
class ScProp prop where
  scProperty :: [String] -> prop -> Q.Property
  qcProperty :: prop -> Q.Property

-- | Instance without preconditions.
instance ScProp Bool where
  scProperty _ res = Q.property res
  qcProperty       = Q.property

-- | Wrapped properties.
instance ScProp ScProperty where
  scProperty _ (Simple res)     = Q.property res
  scProperty _ (Implies prop)   = Q.property $ toQCImp prop

  qcProperty   (Simple res)     = Q.property res
  qcProperty   (Implies prop)   = Q.property $ toQCImp prop

-- | Beta-reduction.
instance (Q.Arbitrary a, Q.Testable prop, Show a, Read a, ScProp prop)
  => ScProp (a -> prop) where
  scProperty (str:strs) f = Q.property $ scProperty strs (f (read str))
  scProperty _          _ = errorMsg "Insufficient values applied to property!"
  qcProperty              = Q.property

propifyWithArgs :: (Read a, ScProp prop)
  => [String] -> (a -> prop) -> (a -> Q.Property)
propifyWithArgs strs prop = scProperty strs . prop

propify :: ScProp prop => (a -> prop) -> (a -> Q.Property)
propify prop = qcProperty . prop

--------------------------------------------------------------------------------
