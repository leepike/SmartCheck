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

import qualified Test.QuickCheck as Q

---------------------------------------------------------------------------------

constrsGen :: (SubTypes a, Generic a, ConNames (Rep a)) 
           => ScArgs -> a -> (a -> Q.Property) -> IO [Idx]
constrsGen args d prop = do
  putStrLn ""
  smartPrtLn "Extrapolating Constructors ..."
  (_, idxs) <- iter' forest (Idx 0 0) []
  return idxs

  where
  forest = mkSubstForest d ()
  iter'  = iter d test next prop

  test x idx = extrapolateConstrs args x idx prop 
  next _ res _ idx idxs = 
    iter' forest idx { column = column idx + 1 } idxs'

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
    -- putStrLn (show constrs) -- YYY
    -- >>
    if S.fromList (conNames a) `S.isSubsetOf` constrs
      then return True
      else do v <- arbSubset args a idx notProp constrs
              case v of
                Result x    -> do -- YYY
                                  putStr $ show idx ++ " "
                                  putStrLn (show x)
                                  recConstrs (subConstr x idx `S.insert` constrs)
                _           -> return False

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
