{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | List properties

module Main where


#if defined(qc) || defined(qcGen) || defined(smart)
import Test
import System.Environment
#endif

import Test.QuickCheck
import Test.SmartCheck

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

#if defined(qc) || defined(qcGen) || defined(smart)
main :: IO ()
main = do
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#if defined(qc) || defined(qcGen)
  test file rnds $ runQC' (proxy :: Proxy [Int]) stdArgs prop_rev length
#else
  test file rnds $ runSC scStdArgs prop_rev length
#endif
#endif

--------------------------------------------------------------------------------
