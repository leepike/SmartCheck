{-# LANGUAGE FlexibleContexts #-}

module Test.SmartCheck.ConstructorGen
  ( constrsGen
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen
import Test.SmartCheck.Render

import Generics.Deriving
import qualified Data.Set as S
import Data.List

import qualified Test.QuickCheck as Q

---------------------------------------------------------------------------------

-- | Entry point to generalize constructors.  We pass in a list of indexes from
-- value generalizations so we don't try to generalize those constructors (or
-- anything below).
constrsGen :: (SubTypes a, Generic a, ConNames (Rep a)) 
           => ScArgs -> a -> (a -> Q.Property) -> [Idx] -> IO [Idx]
constrsGen args d prop vs = do
  putStrLn ""
  smartPrtLn "Extrapolating Constructors ..."
  (_, idxs) <- iter' forest (Idx 0 0) []
  return idxs

  where
  forest     = let forest' = mkSubstForest d True in
               -- This ensures we don't try to replace anything below the indexs
               -- from vs.  It does NOT ensure we don't replace equal indexes.
               foldl' (\f idx -> forestReplaceChildren f idx False) forest' vs

  iter'      = iter d test next prop

  -- Check if this has been generalized already during extrapolating values.
  test x idx = do res <- extrapolateConstrs args x idx prop
                  return $ (not $ idx `elem` vs) && res
                  
  -- Control-flow.  
  next _ res forest' idx idxs = 
    iter' (if res then forestReplaceChildren forest' idx False else forest') 
      idx { column = column idx + 1 } idxs'

    where
    idxs' = if res then idx : idxs else idxs

---------------------------------------------------------------------------------

-- | Return True if we can generalize; False otherwise.
extrapolateConstrs :: (SubTypes a, Generic a, ConNames (Rep a)) 
  => ScArgs -> a -> Idx -> (a -> Q.Property) -> IO Bool
extrapolateConstrs args a idx prop = recConstrs (S.singleton $ subConstr a idx)

  where
  notProp = Q.expectFailure . prop

  recConstrs :: S.Set String -> IO Bool
  recConstrs constrs =
    -- Check if every possible constructor is an element of constrs passed
    -- in.
    if S.fromList (conNames a) `S.isSubsetOf` constrs
      then return True
      else do v <- arbSubset args a idx notProp constrs
              case v of
                Result x      -> recConstrs (subConstr x idx `S.insert` constrs)
                FailedPreCond -> return False
                FailedProp    -> return False

---------------------------------------------------------------------------------

-- | For a value a (used just for typing), and a list of representations of
-- constructors cs, arbSubset generages a new value b, if possible, such that b
-- has the same type as a, and b's constructor is not found in cs.
--
-- Assumes there is some new constructor to test with.
arbSubset :: (SubTypes a, Generic a, ConNames (Rep a)) 
          => ScArgs -> a -> Idx -> (a -> Q.Property) 
          -> S.Set String -> IO (Result a)
arbSubset args a idx prop constrs = 
      -- Because we're looking for some failure that passes the precondition, we
      -- use maxDiscard.
      iterateArb a idx (maxFailure args) (Q.maxSize $ qcArgs args) prop' 
  >>= return

  where
  prop' b = newConstr b Q.==> prop b
  -- Make sure b's constructor is a new one.
  newConstr b = not $ subConstr b idx `S.member` constrs
  
---------------------------------------------------------------------------------

subConstr :: SubTypes a => a -> Idx -> String
subConstr x idx = 
  case getAtIdx x idx of
    Nothing -> errorMsg "constrs'"
    Just x' -> subTconstr x'

  where
  subTconstr (SubT v) = toConstr v

---------------------------------------------------------------------------------
