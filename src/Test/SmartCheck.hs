{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Interface module.

module Test.SmartCheck
  ( smartCheck
  , runQC
  , SubTypes(..)
  , module Test.SmartCheck.Args
  -- ** For constructing new instances of `SubTypes`
  , gst
  , grc
  , gtc
  , gsf
  , gsz
  ) where

import Test.SmartCheck.Args
import Test.SmartCheck.Types
import Test.SmartCheck.Matches
import Test.SmartCheck.Reduce
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Render
import Test.SmartCheck.ConstructorGen

import qualified Test.QuickCheck as Q

import Generics.Deriving

--------------------------------------------------------------------------------

-- | Main interface function.
smartCheck :: forall a prop.
  ( Read a, Q.Arbitrary a, SubTypes a
  , Generic a, ConNames (Rep a)
  , ScProperty prop, Q.Testable prop
  ) => ScArgs -> (a -> prop) -> IO ()
smartCheck args scProp = do
  -- Run standard QuickCheck or read in value.
  (mcex, prop) <-
    if qc args then runQCInit (qcArgs args) scProp
      else do smartPrtLn "Input value to SmartCheck:"
              mcex <- fmap Just (readLn :: IO a)
              return (mcex, propify scProp)

  smartPrtLn $
    "(If any stage takes too long, try modifying the standard "
      ++ "arguments (see Args.hs).)"
  runSmartCheck prop mcex

  where
  runSmartCheck :: (a -> Q.Property) -> Maybe a -> IO ()
  runSmartCheck origProp = smartCheck' [] origProp
    where
    smartCheck' :: [(a, Replace Idx)]
                -> (a -> Q.Property)
                -> Maybe a
                -> IO ()
    smartCheck' ds prop mcex = do
      maybe (maybeDoneMsg >> return ()) go mcex
      where
      go cex = do
          -- Run the smart reduction algorithm.
        d <- smartRun args cex prop
        -- If we asked to extrapolate values, do so.
        valIdxs <- forallExtrap args d origProp
        -- If we asked to extrapolate constructors, do so, again with the
        -- original property.
        csIdxs <- existsExtrap args d valIdxs origProp

        let replIdxs = Replace valIdxs csIdxs
        -- If either kind of extrapolation pass yielded fruit, prettyprint it.
        showExtrapOutput args valIdxs csIdxs replIdxs d

        -- Ask the user if she wants to try again.
        runAgainMsg
        s <- getLine

        if s == ""
          -- If so, then loop, with the new prop.
          then do let oldVals  = (d,replIdxs):ds
                  let matchesProp a =
                              not (matchesShapes a oldVals)
                        Q.==> prop a
                  mcex' <- runQC (qcArgs args) matchesProp
                  smartCheck' oldVals matchesProp mcex'
          else smartPrtLn "Done."

  maybeDoneMsg = smartPrtLn "No value to smart-shrink; done."

--------------------------------------------------------------------------------

existsExtrap :: (Generic a, SubTypes a, ConNames (Rep a))
             => ScArgs -> a -> [Idx] -> (a -> Q.Property) -> IO [Idx]
existsExtrap args d valIdxs origProp =
  if runExists args
    then constrsGen args d origProp valIdxs
    else return []

--------------------------------------------------------------------------------

forallExtrap :: SubTypes a => ScArgs -> a -> (a -> Q.Property) -> IO [Idx]
forallExtrap args d origProp =
  if runForall args
    then -- Extrapolate with the original property to see if we
         -- get a previously-visited value back.
         extrapolate args d origProp
    else return []

--------------------------------------------------------------------------------

showExtrapOutput :: SubTypes a1
                 => ScArgs -> [a] -> [a] -> Replace Idx -> a1 -> IO ()
showExtrapOutput args valIdxs csIdxs replIdxs d =
  if (runForall args || runExists args) && (not $ null (valIdxs ++ csIdxs))
    then output
    else smartPrtLn "Could not extrapolate a new value."
  where
  output = do
    putStrLn ""
    smartPrtLn "Extrapolated value:"
    renderWithVars (format args) d replIdxs

--------------------------------------------------------------------------------

runAgainMsg :: IO ()
runAgainMsg = putStrLn $
     "\nAttempt to find a new counterexample?\n"
  ++ "  ('Enter' to continue;"
  ++ " any character then 'Enter' to quit.)"

--------------------------------------------------------------------------------

runQCInit :: (Show a, Read a, Q.Arbitrary a, ScProperty prop, Q.Testable prop)
          => Q.Args -> (a -> prop) -> IO (Maybe a, a -> Q.Property)
runQCInit args scProp = do
  let prop = propify scProp
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- Q.quickCheckWithResult args genProp
  case res of
    -- XXX C'mon, QuickCheck, let me grab the result in a sane way rather than
    -- parsing a string!
    Q.Failure _ _ _ _ _ _ out -> do
      let outs = lines out
      if length outs < 2 then errorMsg "No value to SmartCheck!"
        else do let cexs = tail outs
                let prop' = propifyWithArgs (tail cexs) scProp
                return (Just $ read $ head cexs, prop')
    -- 2nd arg should never be evaluated if the first arg is Nothing.
    _ -> return (Nothing, errorMsg "Bug in runQCInit")

runQC :: (Show a, Read a, Q.Arbitrary a)
      => Q.Args -> (a -> Q.Property) -> IO (Maybe a)
runQC args prop = do
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- Q.quickCheckWithResult args genProp
  case res of
    -- XXX C'mon, QuickCheck, let me grab the result in a sane way rather than
    -- parsing a string!
    Q.Failure _ _ _ _ _ _ out -> do
      let outs = lines out
      if length outs /= 2 then errorMsg "No value to SmartCheck!"
        else do let cexs = tail outs
                return $ Just (read $ head cexs)
    _ -> return Nothing

--------------------------------------------------------------------------------

-- | Turn a function that returns a `Bool` into a QuickCheck `Property`.
class ScProperty prop where
  scProperty :: [String] -> prop -> Q.Property
  qcProperty :: prop -> Q.Property

-- | Instance without preconditions.
instance ScProperty Bool where
  scProperty _ res = Q.property res
  qcProperty       = Q.property

-- | Instance with a precondition and a postcondition.
instance ScProperty (Bool,Bool) where
  scProperty _ (pre,post) = Q.property $ pre Q.==> post
  qcProperty   (pre,post) = Q.property $ pre Q.==> post

-- | Beta-reduction.
instance (Q.Arbitrary a, Q.Testable prop, Show a, Read a, ScProperty prop)
  => ScProperty (a -> prop) where
  scProperty (str:strs) f = Q.property $ scProperty strs (f (read str))
  scProperty _          _ = errorMsg "Insufficient values applied to property!"
  qcProperty              = Q.property

propifyWithArgs :: (Read a, ScProperty prop)
  => [String] -> (a -> prop) -> (a -> Q.Property)
propifyWithArgs strs prop = \a -> scProperty strs (prop a)

propify :: ScProperty prop => (a -> prop) -> (a -> Q.Property)
propify prop = \a -> qcProperty (prop a)

--------------------------------------------------------------------------------
