{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Degenerate test case.  In which QuickCheck will fail to find a minimal
-- counterexample, but SmartCheck finds one. The reason is that when (generic)
-- shrinking, QuickCheck only uses constructors found in the original
-- counterexample.

module Main where

import Prelude hiding (showList, mod)

#if defined(qcGen) || defined(smart)
-- import Test
import System.Environment
#endif

import Test.QuickCheck
import Test.SmartCheck

import GHC.Generics hiding (S)
import Data.Typeable
import Control.Applicative

-----------------------------------------------------------------

data E = A E | B E | C E | N deriving (Show, Read, Eq, Typeable, Generic)

instance SubTypes E

-- | Generates sequences of 2-100 constructors, wtih a high probability that
-- they're the same.
instance Arbitrary E where
  arbitrary = go
    where go = go' =<< choose (10::Int, 100)
          go' i = do go'' i =<< choose (0::Int, 2)

          go'' 0 _ = return N
          go'' i c = let g = go'' (i-1) in
                     case c of
                       0 -> frequency [ (90, A <$> g 0)
                                      , ( 5, B <$> g 1)
                                      , ( 5, C <$> g 2)
                                      ]
                       1 -> frequency [ (90, B <$> g 1)
                                      , ( 5, A <$> g 0)
                                      , ( 5, C <$> g 2)
                                      ]
                       2 -> frequency [ (90, C <$> g 2)
                                      , ( 5, A <$> g 0)
                                      , ( 5, B <$> g 1)
                                      ]

#ifdef qcGen
  shrink = genericShrink
#endif

-- | The length of the sequence if all constructors are the same, Nothing otherwise.
degenLen :: E -> Maybe Int
degenLen N = Nothing
degenLen e' = case e' of
  A e -> degenA e
  B e -> degenB e
  C e -> degenC e
  N   -> Nothing
  where
  degenA N     = Just 1
  degenA (A e) = fmap (+1) (degenA e)
  degenA _     = Nothing

  degenB N     = Just 1
  degenB (B e) = fmap (+1) (degenB e)
  degenB _     = Nothing

  degenC N     = Just 1
  degenC (C e) = fmap (+1) (degenC e)
  degenC _     = Nothing

-- | Fails if there are more than 4 constructors that are the same.
prop_degen :: E -> Bool
prop_degen e0
  | Just i <- degenLen e0
  , i > 4
  = False
  | otherwise
  = True

-- | Fails if all constructors in a sequence differ and has a size of at least
-- 1.
diff :: E -> Bool
diff N             = True
diff (A (B (C N))) = False
diff (A (C (B N))) = False
diff (B (A (C N))) = False
diff (B (C (A N))) = False
diff (C (A (B N))) = False
diff (C (B (A N))) = False
diff e = size e >= 3

-- Fails if either all values differ or we have a long string of the same
-- constructor.
prop :: E -> Bool
prop e = prop_degen e && diff e

size :: E -> Int
size = sizeE

sizeE :: E -> Int
sizeE e' = case e' of
  A e -> 1 + sizeE e
  B e -> 1 + sizeE e
  C e -> 1 + sizeE e
  N   -> 0

qArgs :: Args
qArgs = stdArgs { maxSuccess = 10000 }

scargs :: ScArgs
scargs = scStdArgs { qcArgs  = qArgs
                   , format  = PrintString
                   , runForall   = False
                   , runExists   = False
                   }

qcTest :: IO ()
qcTest = quickCheckWith stdArgs { maxSuccess = 10000 } prop

scTest :: IO ()
scTest = smartCheck scargs prop

-- #if defined(qcGen) || defined(smart)
-- main :: IO ()
-- main = do
--   [file', rnds'] <- getArgs
--   let rnds = read rnds' :: Int
--   let file  = read file' :: String
-- #if defined(qcGen)
--   test file rnds $ runQC' proxy qArgs prop size
-- #endif
-- #ifdef smart
--   test file rnds $ runSC scargs prop size
-- #endif
-- #endif
