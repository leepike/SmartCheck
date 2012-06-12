{-# LANGUAGE FlexibleContexts #-}

module Test.SmartCheck.ConstructorGen
  ( --NewArb(..)
    recConstrs
--    arbSubset
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen

import Generics.Deriving
import qualified Data.Set as S

import qualified Test.QuickCheck as Q
--import qualified Test.QuickCheck.Gen as Q

---------------------------------------------------------------------------------

-- | For a value a (used just for typing), and a list of representations of
-- constructors cs, arbSubset generages a new value b, if possible, such that b
-- has the same type as a, and b's constructor is not found in cs.
arbSubset :: (SubTypes a, Generic a, ConNames (Rep a)) 
          => Q.Args -> a -> Idx -> (a -> Q.Property) 
          -> [String] -> IO (Maybe (Result a))
arbSubset args a idx prop constrs = 
  -- Check if every possible constructor is an element of constrs passed in.
  if S.fromList (conNames a) `S.isSubsetOf` S.fromList constrs
    then return Nothing
    else iterateArb a idx (Q.maxSuccess args) (Q.maxSize args) prop' >>=
           (return . Just)

  where
  prop' b = newConstr b Q.==> prop b
  -- Make sure b's constructor is a new one.
  newConstr b = not $ toConstr b `elem` constrs
  
---------------------------------------------------------------------------------

-- | Return True if we can generalize; False otherwise.
recConstrs :: (SubTypes a, Generic a, ConNames (Rep a)) 
           => Q.Args -> a -> Idx -> (a -> Q.Property) 
           -> [String] -> IO Bool
recConstrs args a idx prop constrs = do
  v <- arbSubset args a idx prop constrs
  case v of
    Nothing            -> return True
    Just (Result x)    -> recConstrs args a idx prop (constrs' x)
    Just _             -> return False

  where
  constrs' x = subVal x : constrs

  subVal x = case getAtIdx x idx of
               Nothing -> errorMsg "recConstrs"
               Just x' -> subTconstr x'
  
  subTconstr (SubT v) = toConstr v
