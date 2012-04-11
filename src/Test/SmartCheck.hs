-- | Interface module.

module Test.SmartCheck 
  ( SmartArgs (..)
  , stdSmartArgs 
  , smartRun
  , extrapolate
  -- SubTypes class
  , SubT(..)
  , subT
  , SubTypes(..)
  ) where

import Test.SmartCheck.Reduce
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Types
