{-# LANGUAGE FlexibleContexts #-}

module Test.SmartCheck.ConstructorGen
  ( --NewArb(..)
    extrapolateConstrs
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
--
-- Assumes there is some new constructor to test with.
arbSubset :: (SubTypes a, Generic a, ConNames (Rep a)) 
          => Q.Args -> a -> Idx -> (a -> Q.Property) 
          -> S.Set String -> IO (Result a)
arbSubset args a idx prop constrs = 
      iterateArb a idx (Q.maxSuccess args) (Q.maxSize args) prop' 
  >>= return

  where
  prop' b = newConstr b Q.==> prop b
  -- Make sure b's constructor is a new one.
  newConstr b = not $ toConstr b `S.member` constrs
  
---------------------------------------------------------------------------------

-- | Return True if we can generalize; False otherwise.
extrapolateConstrs :: (SubTypes a, Generic a, ConNames (Rep a)) 
  => Q.Args -> a -> Idx -> (a -> Q.Property) -> IO Bool
extrapolateConstrs args a idx prop = recConstrs (constrs' a S.empty)

  where
  notProp = Q.expectFailure . prop

  recConstrs constrs =
    -- Check if every possible constructor is an element of constrs passed
    -- in.
    putStrLn (show constrs) -- YYY
    >>
    if S.fromList (conNames a) `S.isSubsetOf` constrs
      then return True
      else do v <- arbSubset args a idx notProp constrs
              case v of
                Result x    -> if recConstrs $ constrs' x constrs
                _           -> return False

  constrs' x constrs = subVal `S.insert` constrs
    where
    subVal = case getAtIdx x idx of
               Nothing -> errorMsg "constrs'"
               Just x' -> subTconstr x'

    subTconstr (SubT v) = toConstr v

---------------------------------------------------------------------------------
