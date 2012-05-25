{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module MutualRecData where

import Test.SmartCheck
import Test.QuickCheck hiding (Result)

import Data.Tree
import Control.Monad.State 

import GHC.Generics

---------------------------------------------------------------------------------

data M = M N N Int
       | P
  deriving (Typeable, Show, Eq, Read, Generic)

instance SubTypes M

data N = N M Int String
  deriving (Typeable, Show, Eq, Read, Generic)

instance SubTypes N

---------------------------------------------------------------------------------

instance Arbitrary M where
  arbitrary = 
    sized $ \n -> if n == 0 then return P 
                    else oneof [ return P
                               , liftM3 M (resize (n-1) arbitrary) 
                                          (resize (n-1) arbitrary) 
                                          arbitrary 
                               ]

instance Arbitrary N where
  arbitrary = liftM3 N arbitrary arbitrary arbitrary

---------------------------------------------------------------------------------

prop0 :: M -> Bool
prop0 (M _ _ a) = a < 100
prop0 _         = True

mutRecTest :: IO ()
mutRecTest = smartCheck args p
  where 
  p    = \a -> property (prop0 a)
  args = scStdArgs { qcArgs = stdArgs {maxSuccess = 1000} }

---------------------------------------------------------------------------------

testProp :: M -> Bool
testProp (M _ _ i) = i > 100
testProp _         = True

xx :: M
xx = M (N (M (N P 1 "goo") (N P 7 "foo") 8) 3 "hi") (N P 4 "bye") 6    
yy :: Forest Int
yy = [Node 0 [Node 1 [], Node 2 []], Node 3 [Node 4 [], Node 5 [Node 6 []]]]

---------------------------------------------------------------------------------
