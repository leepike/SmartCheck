{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface module.

module Test.SmartCheck
  ( smartCheck
  , runQC
  , SubTypes
  , module Test.SmartCheck.Args
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
smartCheck :: forall a b. ( Read a, Q.Arbitrary a, SubTypes a
                          , Generic a, ConNames (Rep a), Q.Testable b )
    => ScArgs -> (a -> b) -> IO ()
smartCheck args propT = do
  smartPrtLn $
    "If any stage takes too long to reduce, try reducing the standard "
      ++ "arguments (see Args.hs)."
  smartCheck' prop []

  where
  prop a = Q.property $ propT a

  smartCheck' :: (a -> Q.Property) -> [(a, Replace Idx)] -> IO ()
  smartCheck' prop' ds = do
    -- Run standard QuickCheck or read in value.
    res <- if qc args then runQC (qcArgs args) prop'
             else do smartPrtLn "Input value to SmartCheck:"
                     fmap Just (readLn :: IO a)
    maybe (smartPrtLn "No value to smart-shrink; done.") go res

    where
    go r = do
      -- Run the smart reduction algorithm.
      d   <- smartRun args r prop'
      -- If we asked to extrapolate values, do so.
      valIdxs <- if extrap args
                   then -- Extrapolate with the original property to see if we
                        -- get a previously-visited value back.
                        extrapolate args d prop
                   else return []

      -- If we asked to extrapolate constructors, do so, again with the original
      -- property.
      csIdxs <- if constrGen args
                  then constrsGen args d prop valIdxs
                  else return []

      let replIdxs = Replace valIdxs csIdxs

      -- If either kind of extrapolation pass yielded fruit, prettyprint it.
      if not $ null (valIdxs ++ csIdxs)
        then output d replIdxs
        else smartPrtLn "Could not extrapolate a new value."

      -- Ask the user if she wants to try again.
      putStrLn $ "Attempt to find a new counterexample?\n"
                   ++ "  ('Enter' to continue;"
                   ++ " any character then 'Enter' to quit.)"
      s <- getLine

      let oldVals  = (d,replIdxs):ds
      let matchesProp a = not (matchesShapes a oldVals) Q.==> prop' a

      if s == ""
        -- If so, then loop, with the new prop.
        then smartCheck' matchesProp oldVals
        else smartPrtLn "Done."

      where
      output :: a -> Replace Idx -> IO ()
      output d repl = do
        putStrLn ""
        smartPrtLn "Extrapolated value:"
        renderWithVars (format args) d repl

--------------------------------------------------------------------------------

runQC :: forall a. (Show a, Read a, Q.Arbitrary a)
      => Q.Args -> (a -> Q.Property) -> IO (Maybe a)
runQC args prop = do
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- Q.quickCheckWithResult args genProp
  case res of
    -- XXX C'mon, QuickCheck, let me grab the result in a sane way rather than
    -- parsing a string!
    Q.Failure _ _ _ _ _ _ out -> do let ms = lines out !! 1
                                    let m  = read ms :: a
                                    return $ Just m
    _ -> return Nothing

--------------------------------------------------------------------------------
