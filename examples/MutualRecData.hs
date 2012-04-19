{-# LANGUAGE DeriveDataTypeable #-}

module MutualRecData where

import Test.SmartCheck
import Test.QuickCheck hiding (Result)

import Data.Data
import Control.Monad.State 

---------------------------------------------------------------------------------

data M = M N N Int
       | P
  deriving (Data, Typeable, Show, Eq, Read)

data N = N M Int String
  deriving (Data, Typeable, Show, Eq, Read)

data O = O N String
  deriving (Data, Typeable, Show, Eq, Read)

---------------------------------------------------------------------------------

instance SubTypes M where
  subTypes (M n0 n1 j) = 
    [ Node (subT n0) (subTypes n0)
    , Node (subT n1) (subTypes n1)
    , Node (subT j)  []
    ] 
  subTypes P = []

instance SubTypes N where
  subTypes (N m i s) = 
    [ Node (subT m) (subTypes m)
    , Node (subT i) []
    , Node (subT s) []
    ]

instance SubTypes O where
  subTypes (O n s) = 
    [ Node (subT n) (subTypes n)
    , Node (subT s) []
    ]

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

main :: IO ()
main = do result <- smartRun args p
          extrapolate args result p
  where 
  p = \a -> property (prop0 a)
  args = stdArgs { maxSuccess = 1000 }

---------------------------------------------------------------------------------

testProp :: M -> Bool
testProp (M _ _ i) = i > 100
testProp _         = True

xx :: M
xx = M (N (M (N P 1 "goo") (N P 7 "foo") 8) 3 "hi") (N P 4 "bye") 6    
yy :: Forest Int
yy = [Node 0 [Node 1 [], Node 2 []], Node 3 [Node 4 [], Node 5 [Node 6 []]]]

---------------------------------------------------------------------------------
