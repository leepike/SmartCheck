{-# LANGUAGE FlexibleContexts #-}

module Test.SmartCheck.ConstructorGen
  ( constrsGen
  ) where

import Test.SmartCheck.Args
import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree
import Test.SmartCheck.SmartGen
import Test.SmartCheck.Render

import Prelude hiding (max)
import Generics.Deriving
import qualified Data.Set as S
import Data.List
import Control.Monad (liftM)

import qualified Test.QuickCheck as Q

--------------------------------------------------------------------------------

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

  iter'      = iter d test next prop (scMaxDepth args)

  -- Check if this has been generalized already during extrapolating values.
  test x idx = do res <- extrapolateConstrs args x idx prop
                  return $ idx `notElem` vs && res

  -- Control-flow.
  next _ res forest' idx idxs =
    iter' (if res then forestReplaceChildren forest' idx False else forest')
      idx { column = column idx + 1 } idxs'

    where
    idxs' = if res then idx : idxs else idxs

--------------------------------------------------------------------------------

-- | Return True if we can generalize; False otherwise.
extrapolateConstrs :: (SubTypes a, Generic a, ConNames (Rep a))
  => ScArgs -> a -> Idx -> (a -> Q.Property) -> IO Bool
extrapolateConstrs args a idx prop =
  recConstrs $ S.singleton $ subConstr a idx $ scMaxDepth args
  where
  notProp = Q.expectFailure . prop
  allConstrs = S.fromList (conNames a)

  recConstrs :: S.Set String -> IO Bool
  recConstrs constrs =
    let newConstr x = subConstr x idx (scMaxDepth args) `S.insert` constrs in
    -- Check if every possible constructor is an element of constrs passed in.
    if allConstrs `S.isSubsetOf` constrs
      then return True
      else do v <- arbSubset args a idx notProp constrs
              case v of
                Result x      -> recConstrs (newConstr x)
                FailedPreCond -> return False
                FailedProp    -> return False
                BaseType      -> return False

--------------------------------------------------------------------------------

-- | For a value a (used just for typing), and a list of representations of
-- constructors cs, arbSubset generages a new value b, if possible, such that b
-- has the same type as a, and b's constructor is not found in cs.
--
-- Assumes there is some new constructor to test with.
arbSubset :: (SubTypes a, Generic a, ConNames (Rep a))
          => ScArgs -> a -> Idx -> (a -> Q.Property)
          -> S.Set String -> IO (Result a)
arbSubset args a idx prop constrs =
  liftM snd $ iterateArbIdx a (idx, scMaxDepth args)
                (scMaxExists args) (scMaxSize args) prop'
  where
  prop' b = newConstr b Q.==> prop b
  -- Make sure b's constructor is a new one.
  newConstr b = not $ subConstr b idx (scMaxDepth args) `S.member` constrs

--------------------------------------------------------------------------------

-- | Get the constructor at an index in x.
subConstr :: SubTypes a => a -> Idx -> Maybe Int -> String
subConstr x idx max =
  case getAtIdx x idx max of
    Nothing -> errorMsg "constrs'"
    Just x' -> subTconstr x'

  where
  subTconstr (SubT v) = toConstr v

--------------------------------------------------------------------------------
