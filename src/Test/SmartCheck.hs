{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Interface module.

module Test.SmartCheck
  ( -- ** Main interface function.
    smartCheck

  -- ** Type of SmartCheck properties.
  , ScProp()
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

-- XXX I have to parse a string from QC to get the counterexamples.

-- | Run QuickCheck initially, to get counterexamples for each argument,
-- includding the one we want to focus on for SmartCheck, plus a `Property`.
runQCInit :: (Show a, Read a, Q.Arbitrary a, ScProperty prop, Q.Testable prop)
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
    Q.Failure _ _ _ _ _ _ _ out -> Just $ lines out
    _                           -> Nothing

genProp :: (Show a, Q.Testable prop, Q.Arbitrary a)
        => (a -> prop) -> Q.Property
genProp prop = Q.forAllShrink Q.arbitrary Q.shrink prop

--------------------------------------------------------------------------------

-- | Type for SmartCheck properties.  Moral equivalent of QuickCheck's
-- `Property` type.
data ScProp = Implies (Bool, Bool)
            | Simple  Bool
  deriving (Show, Read, Eq)

instance Q.Testable ScProp where
  property (Simple prop)         = Q.property prop
  property (Implies prop)        = Q.property (toQCImp prop)
  exhaustive (Simple prop)       = Q.exhaustive prop
  exhaustive (Implies prop)      = Q.exhaustive (toQCImp prop)

-- same as ==>
infixr 0 -->
-- | Moral equivalent of QuickCheck's `==>` operator.
(-->) :: Bool -> Bool -> ScProp
pre --> post = Implies (pre, post)

-- Helper function.
toQCImp :: (Bool, Bool) -> Q.Property
toQCImp (pre, post) = pre Q.==> post

-- | Turn a function that returns a `Bool` into a QuickCheck `Property`.
class ScProperty prop where
  scProperty :: [String] -> prop -> Q.Property
  qcProperty :: prop -> Q.Property

-- | Instance without preconditions.
instance ScProperty Bool where
  scProperty _ res = Q.property res
  qcProperty       = Q.property

-- | Wrapped properties.
instance ScProperty ScProp where
  scProperty _ (Simple res)     = Q.property res
  scProperty _ (Implies prop)   = Q.property $ toQCImp prop

  qcProperty   (Simple res)     = Q.property res
  qcProperty   (Implies prop)   = Q.property $ toQCImp prop

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
