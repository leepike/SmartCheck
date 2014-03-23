{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Interface module.

module Test.SmartCheck
  ( -- ** Main SmartCheck interface.
    smartCheck

    -- ** User-suppplied counterexample interface.
  , smartCheckInput

  -- ** Run QuickCheck and get a result.
  , runQC

  -- ** Arguments
  , module Test.SmartCheck.Args

  -- ** Main type class based on Generics.
  , SubTypes(..)

  -- ** For constructing new instances of `SubTypes`
  , gst
  , grc
  , gtc
  , gsf
  , gsz
  ) where

import Test.SmartCheck.Args
import Test.SmartCheck.ConstructorGen
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Matches
import Test.SmartCheck.Reduce
import Test.SmartCheck.Render
import Test.SmartCheck.Test
import Test.SmartCheck.Types

import qualified Test.QuickCheck as Q

import Generics.Deriving
import Control.Monad (when)

--------------------------------------------------------------------------------

-- | Main interface function.
smartCheck ::
  ( SubTypes a
  , Generic a, ConNames (Rep a)
  , Q.Testable prop
  ) => ScArgs -> (a -> prop) -> IO ()
smartCheck args scProp =
  smartCheckRun args =<< runQC (qcArgs args) scProp

smartCheckInput :: forall a prop.
  ( SubTypes a
  , Generic a, ConNames (Rep a)
  , Q.Testable prop
  , Read a
  ) => ScArgs -> (a -> prop) -> IO ()
smartCheckInput args scProp = do
  smartPrtLn "Input value to SmartCheck:"
  mcex <- fmap Just (readLn :: IO a)
  smartCheckRun args (mcex, Q.property . scProp)

smartCheckRun :: forall a.
  ( SubTypes a
  , Generic a, ConNames (Rep a)
  ) => ScArgs -> (Maybe a, a -> Q.Property) -> IO ()
smartCheckRun args (origMcex, origProp) = do
  smartPrtLn $
    "(If any stage takes too long, try modifying SmartCheck's standard "
      ++ "arguments (see Args.hs).)"
  smartCheck' [] origMcex origProp
  where
  smartCheck' :: [(a, Replace Idx)]
              -> Maybe a
              -> (a -> Q.Property)
              -> IO ()
  smartCheck' ds mcex prop =
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
      -- Try again?
      runAgainMsg

      s <- getLine
      if s == ""
        -- If so, then loop, with the new prop.
        then do let oldVals  = (d,replIdxs):ds
                let matchesProp a =
                            not (matchesShapes a oldVals)
                      Q.==> prop a
                (mcex', _) <- runQC (qcArgs args) (Q.noShrinking . matchesProp)
                smartCheck' oldVals mcex' matchesProp
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
  when (runForall args || runExists args) $ do
    if null (valIdxs ++ csIdxs)
      then smartPrtLn "Could not extrapolate a new value."
      else output
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

-- | Run QuickCheck, to get a counterexamples for each argument, including the
-- one we want to focus on for SmartCheck, which is the first argument.  That
-- argument is never shrunk by QuickCheck, but others may be shrunk by
-- QuickCheck.  Returns the value (if it exists) and a 'Property' (by applying
-- the 'property' method to the 'Testable' value).  In each iteration of
-- 'runQC', non-SmartCheck arguments are not necessarily held constant
runQC :: forall a prop . (Show a, Q.Arbitrary a, Q.Testable prop)
          => Q.Args -> (a -> prop) -> IO (Maybe a, a -> Q.Property)
runQC args scProp = do
  (mCex, res) <- scQuickCheckWithResult args scProp
  return $ if failureRes res
             then (mCex,    Q.property . scProp)
             else (Nothing, Q.property . scProp)

-- | Returns 'True' if a counterexample is returned and 'False' otherwise.
failureRes :: Q.Result -> Bool
failureRes res =
  case res of
    Q.Failure _ _ _ _ _ _ _ _ _ _ -> True
    _                             -> False

--------------------------------------------------------------------------------

