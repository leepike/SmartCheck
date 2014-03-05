{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Interface module.

module Test.SmartCheck
  ( -- ** Main SmartCheck interface.
    smartCheck

  -- ** Type of SmartCheck properties.
  , ScProperty()

  -- ** Implication for SmartCheck properties.
  , (-->)

  -- ** Run QuickCheck and get a result.
  , runQCInit

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
import Test.SmartCheck.Types
import Test.SmartCheck.Matches
import Test.SmartCheck.Property
import Test.SmartCheck.Reduce
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Render
import Test.SmartCheck.ConstructorGen

import qualified Test.QuickCheck as Q

import Generics.Deriving

--------------------------------------------------------------------------------

-- | Main interface function.
smartCheck ::
  ( Read a, SubTypes a
  , Generic a, ConNames (Rep a)
  , ScProp prop
  ) => ScArgs -> (a -> prop) -> IO ()
smartCheck args scProp =
  -- Run standard QuickCheck or read in value.
  smartCheckRun args =<< if qc args
                           then runQCInit (qcArgs args) scProp
                           else smartCheckInput scProp

smartCheckInput :: forall a prop.
  ( Read a, SubTypes a
  , Generic a, ConNames (Rep a)
  , ScProp prop
  ) => (a -> prop) -> IO (Maybe a, a -> Q.Property)
smartCheckInput scProp = do
  smartPrtLn "Input value to SmartCheck:"
  mcex <- fmap Just (readLn :: IO a)
  return (mcex, propify scProp)

smartCheckRun :: forall a.
  ( Read a, SubTypes a
  , Generic a, ConNames (Rep a)
  ) => ScArgs -> (Maybe a, a -> Q.Property) -> IO ()
smartCheckRun args (origMcex, origProp) = do
  smartPrtLn $
    "(If any stage takes too long, try modifying the standard "
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

-- XXX I have to parse a string from QC to get the counterexamples.

-- | Run QuickCheck initially, to get counterexamples for each argument,
-- includding the one we want to focus on for SmartCheck, plus a `Property`.
runQCInit :: (Show a, Read a, Q.Arbitrary a, ScProp prop)
          => Q.Args -> (a -> prop) -> IO (Maybe a, a -> Q.Property)
runQCInit args scProp = do
  res <- Q.quickCheckWithResult args (genProp $ propify scProp)
  return $ maybe
    -- 2nd arg should never be evaluated if the first arg is Nothing.
    (Nothing, errorMsg "Bug in runQCInit")
    ((\(cex, p) -> (Just cex, p)) . parse)
    (getOut res)
  where
  parse outs = (read $ head cexs, prop')
    where cexs = lenChk ((< 2) . length) outs
          prop' = propifyWithArgs (tail cexs) scProp

-- | Run QuickCheck only analyzing the SmartCheck value, holding the other
-- values constant.
runQC :: (Show a, Read a, Q.Arbitrary a)
      => Q.Args -> (a -> Q.Property) -> IO (Maybe a)
runQC args prop = do
  res <- Q.quickCheckWithResult args (genProp prop)
  return $ fmap parse (getOut res)
  where
  parse outs = read $ head cexs
    where cexs = lenChk ((/= 2) . length) outs

lenChk :: ([String] -> Bool) -> [String] -> [String]
lenChk chk ls = if chk ls then errorMsg "No value to SmartCheck!"
                   else tail ls

getOut :: Q.Result -> Maybe [String]
getOut res =
  case res of
    Q.Failure _ _ _ _ _ _ _ _ _ out -> Just $ lines out
    _                               -> Nothing

genProp :: (Show a, Q.Testable prop, Q.Arbitrary a)
        => (a -> prop) -> Q.Property
genProp prop = Q.forAllShrink Q.arbitrary Q.shrink prop

--------------------------------------------------------------------------------

