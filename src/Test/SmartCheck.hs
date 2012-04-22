-- | Interface module.

module Test.SmartCheck 
  ( smartCheck
  , SubT(..)
  , subT
  , SubTypes(..)
  , Tree(..)
  , Forest
  ) where

import Test.SmartCheck.Reduce
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Types
import Test.SmartCheck.Common

import qualified Test.QuickCheck as Q
import Data.Tree
import Data.Maybe

---------------------------------------------------------------------------------

smartCheck :: (Read a, Show a, Q.Arbitrary a, SubTypes a)
           => Q.Args -> (a -> Q.Property) -> IO ()
smartCheck args prop = do
  d <- smartRun args prop
  if isNothing d then return ()
    else do prop' <- extrapolate args (fromJust d) prop
            c <- continue
            if c then smartCheck args  prop'
              else smartPrtLn "Done."

  where
  continue = do putStrLn $ "Attempt to find a new counterexample?" 
                            ++ " (Enter to continue, 'n' to quit.)"
                s <- getLine
                return (s == "")

---------------------------------------------------------------------------------
