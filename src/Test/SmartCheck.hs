{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 

-- | Interface module.

module Test.SmartCheck 
  ( smartCheck
  , module Test.SmartCheck.Types
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.Reduce
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Render
import Test.SmartCheck.ConstructorGen

import qualified Test.QuickCheck as Q

import Data.Maybe
import Generics.Deriving

---------------------------------------------------------------------------------

-- | Main interface function.
smartCheck :: forall a b. ( Read a, Q.Arbitrary a, SubTypes a
                          , Generic a, ConNames (Rep a), Q.Testable b )
    => ScArgs -> (a -> b) -> IO ()
smartCheck args propT = smartCheck' prop []

  where
  prop a = Q.property $ propT a

  smartCheck' :: (a -> Q.Property) -> [a] -> IO ()
  smartCheck' prop' ds = do
    -- Run standard QuickCheck.
    res <- runQC (qcArgs args) prop'
    case res of 
      Nothing -> smartPrtLn "No value to smart-shrink; done." 
      Just r  -> go r

    where
    go r = do
      -- Run the smart reduction algorithm.
      d   <- smartRun args r prop'
      -- If we asked to extrapolate values, do so.
      vs  <- if extrap args
                then -- Extrapolate with the original property to see if we get
                     -- a previously-visited value back.
                     do (idxs, prop_) <- extrapolate args d prop ds
                        return $ Just (idxs, prop_)
                else return Nothing

      -- If we asked to extrapolate constructors, do so.
      cs  <- if constrGen args
               then     constrsGen args d prop (concat . maybeToList $ fmap fst vs) 
                    >>= return . Just
               else return Nothing

      -- If either kind of extrapolation pass yielded fruit, prettyprint it.
      if nonEmpty d vs cs 
        then output d (repls vs cs)
        else smartPrtLn "Could not extrapolate a new value."

      -- Ask the user if she wants to try again.
      putStrLn $ "Attempt to find a new counterexample?" 
                   ++ " ('Enter' to continue;"
                   ++ " any character then 'Enter' to quit.)"
      s <- getLine
      if (s == "")
        -- If so, then loop, with the new prop.
        then smartCheck' (propApp vs $ prop') (d : ds)
        else smartPrtLn "Done."

      where
      output :: a -> Replace Idx -> IO ()
      output d repl = do
        putStrLn ""
        smartPrtLn "Extrapolated value:"
        renderWithVars (treeShow args) d repl -- XXX

      nonEmpty :: a -> Maybe ([Idx], PropRedux a) -> Maybe [c] -> Bool
      nonEmpty d vs cs = vsIdxs || csIdxs
        where
        vsIdxs = case vs of
                   Just (idxs, _) ->    (not $ matchesShapes d ds idxs)
                                     && (not $ null idxs)
                   Nothing        -> False
        csIdxs = case cs of
                   Just idxs -> not $ null idxs
                   Nothing   -> False 

---------------------------------------------------------------------------------

runQC :: forall a. (Show a, Read a, Q.Arbitrary a)
      => Q.Args -> (a -> Q.Property) -> IO (Maybe a)
runQC args prop = do
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- Q.quickCheckWithResult args genProp
  case res of
    -- XXX C'mon, QuickCheck, let me grab the result in a sane way rather than
    -- parsing a string!
    Q.Failure _ _ _ _ _ _ out -> do let ms = (lines out) !! 1 
                                    let m = (read ms) :: a
                                    return $ Just m
    _ -> return Nothing

---------------------------------------------------------------------------------
-- Helpers

repls :: Maybe ([Idx], PropRedux a) -> Maybe [Idx] -> Replace Idx
repls vs cs = Replace v c
  where 
  v = case vs of 
        Nothing        -> []
        Just (idxs, _) -> idxs
  c = case cs of 
        Nothing    -> []
        Just idxs  -> idxs

propApp :: Maybe ([Idx], PropRedux a) -> PropRedux a
propApp vs = case vs of 
               Nothing         -> id
               Just (_, prop_) -> prop_

---------------------------------------------------------------------------------
