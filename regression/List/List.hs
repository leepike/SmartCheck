{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | List properties

module Main where

import Test
import Test.QuickCheck
import Test.SmartCheck
import Control.Monad
import System.Environment

import GHC.Generics
import Data.Typeable

-----------------------------------------------------------------

prop_rev :: [Int] -> Bool
prop_rev ls = reverse ls == ls

revTest :: IO ()
revTest = smartCheck args prop_rev
  where
  args = scStdArgs { qcArgs  = stdArgs
                                -- { maxSuccess = 1000
                                -- , maxSize    = 20  }
                   , format  = PrintString
                   , runForall  = True
                   }

main :: IO ()
main = do
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#ifdef qc
  test file rnds (runQC' stdArgs prop_rev :: IO (Maybe [Int])) length
#else
  test file rnds (runSC scStdArgs prop_rev) length
#endif

--------------------------------------------------------------------------------
