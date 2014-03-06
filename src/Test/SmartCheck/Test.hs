{-

The following is modified by Lee Pike 2014 and still retains the following
license:

Copyright (c) 2000-2012, Koen Claessen
Copyright (c) 2006-2008, Björn Bringert
Copyright (c) 2009-2012, Nick Smallbone
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
- Neither the names of the copyright owners nor the names of the
  contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The main test loop.
module Test.SmartCheck.Test
  ( scQuickCheckWithResult
  , stdArgs
  ) where

--------------------------------------------------------------------------
-- imports

import Prelude hiding (break)

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
import Test.QuickCheck.State
import Test.QuickCheck.Exception
import Test.QuickCheck.Random
import System.Random(split)

import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , group
  , groupBy
  , intersperse
  )

import Test.QuickCheck.Arbitrary

--------------------------------------------------------------------------
-- quickCheck

-- * Running tests

-- | Args specifies arguments to the QuickCheck driver
data Args
  = Args
  { replay          :: Maybe (QCGen,Int) -- ^ Should we replay a previous test?
  , maxSuccess      :: Int               -- ^ Maximum number of successful tests before succeeding
  , maxDiscardRatio :: Int               -- ^ Maximum number of discarded tests per successful test before giving up
  , maxSize         :: Int               -- ^ Size to use for the biggest test cases
  , chatty          :: Bool              -- ^ Whether to print anything
  }
 deriving ( Show, Read )

-- | Result represents the test result
data Result
  -- | A successful test run
  = Success
    { numTests       :: Int               -- ^ Number of tests performed
    , labels         :: [(String,Int)]    -- ^ Labels and frequencies found during all successful tests
    , output         :: String            -- ^ Printed output
    }
  -- | Given up
  | GaveUp
    { numTests       :: Int               --   Number of tests performed
    , labels         :: [(String,Int)]    --   Labels and frequencies found during all successful tests
    , output         :: String            --   Printed output
    }
  -- | A failed test run
  | Failure
    { numTests       :: Int               --   Number of tests performed
    , numShrinks     :: Int               -- ^ Number of successful shrinking steps performed
    , numShrinkTries :: Int               -- ^ Number of unsuccessful shrinking steps performed
    , numShrinkFinal :: Int               -- ^ Number of unsuccessful shrinking steps performed since last successful shrink
    , usedSeed       :: QCGen             -- ^ What seed was used
    , usedSize       :: Int               -- ^ What was the test size
    , reason         :: String            -- ^ Why did the property fail
    , theException   :: Maybe AnException -- ^ The exception the property threw, if any
    , labels         :: [(String,Int)]    --   Labels and frequencies found during all successful tests
    , output         :: String            --   Printed output
    }
  -- | A property that should have failed did not
  | NoExpectedFailure
    { numTests       :: Int               --   Number of tests performed
    , labels         :: [(String,Int)]    --   Labels and frequencies found during all successful tests
    , output         :: String            --   Printed output
    }
 deriving ( Show )

-- | The default test arguments
stdArgs :: Args
stdArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = True
-- noShrinking flag?
  }

scQuickCheckWithResult :: forall a prop. (Show a, Arbitrary a, Testable prop) => Args -> (a -> prop) -> IO (Maybe a, Result)
scQuickCheckWithResult a p = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case replay a of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     test MkState{ terminal                  = tm
                 , maxSuccessTests           = maxSuccess a
                 , maxDiscardedTests         = maxDiscardRatio a * maxSuccess a
                 , computeSize               = case replay a of
                                                 Nothing    -> computeSize'
                                                 Just (_,s) -> computeSize' `at0` s
                 , numSuccessTests           = 0
                 , numDiscardedTests         = 0
                 , numRecentlyDiscardedTests = 0
                 , collected                 = []
                 , expectedFailure           = False
                 , randomSeed                = rnd
                 , numSuccessShrinks         = 0
                 , numTryShrinks             = 0
                 , numTotTryShrinks          = 0
                 } flipProp
  where computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
          | otherwise =
            ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a
        n `roundTo` m = (n `div` m) * m
        at0 _f s 0 0  = s
        at0 f _s n d  = f n d

        flipProp :: QCGen -> Int -> (a -> Prop)
        flipProp q i = \a' ->
          let p' = p a' in
          let g = unGen (unProperty (property p')) in
          g q i

--------------------------------------------------------------------------
-- main test loop

test :: Arbitrary a => State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
test st f
  | numSuccessTests st   >= maxSuccessTests st   = doneTesting st f
  | numDiscardedTests st >= maxDiscardedTests st = giveUp st f
  | otherwise                                    = runATest st f

doneTesting :: State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
doneTesting st _f =
  do -- CALLBACK done_testing?
     if expectedFailure st then
       putPart (terminal st)
         ( "+++ OK, passed "
        ++ show (numSuccessTests st)
        ++ " tests"
         )
      else
       putPart (terminal st)
         ( bold ("*** Failed!")
        ++ " Passed "
        ++ show (numSuccessTests st)
        ++ " tests (expected failure)"
         )
     success st
     theOutput <- terminalOutput (terminal st)
     return $ (Nothing, if expectedFailure st then
                          Success{ labels = summary st,
                                   numTests = numSuccessTests st,
                                   output = theOutput }
                        else NoExpectedFailure{ labels = summary st,
                                                numTests = numSuccessTests st,
                                                output = theOutput })

giveUp :: State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
giveUp st _f =
  do -- CALLBACK gave_up?
     putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (numSuccessTests st)
      ++ " tests"
       )
     success st
     theOutput <- terminalOutput (terminal st)
     return (Nothing, GaveUp{ numTests = numSuccessTests st
                            , labels   = summary st
                            , output   = theOutput
                            })

runATest :: forall a. (Arbitrary a) => State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
runATest st f =
  do -- CALLBACK before_test
     putTemp (terminal st)
        ( "("
       ++ number (numSuccessTests st) "test"
       ++ concat [ "; " ++ show (numDiscardedTests st) ++ " discarded"
                 | numDiscardedTests st > 0
                 ]
       ++ ")"
        )
     let size = computeSize st (numSuccessTests st) (numRecentlyDiscardedTests st)

     let p :: a -> Prop
         p = f rnd1 size

     let genA :: QCGen -> Int -> a
         genA = unGen arbitrary
     let rndA = genA rnd1 size

     let mkRes a = return (Just rndA, a)

     MkRose res ts <- protectRose (reduceRose (unProp (p rndA)))
     callbackPostTest st res

     let continue break st' | abort res = break st'
                            | otherwise = test st'

     case res of
       MkResult{ok = Just True, stamp, expect} -> -- successful test
         do continue doneTesting
              st{ numSuccessTests           = numSuccessTests st + 1
                , numRecentlyDiscardedTests = 0
                , randomSeed                = rnd2
                , collected                 = stamp : collected st
                , expectedFailure           = expect
                } f

       MkResult{ok = Nothing, expect = expect} -> -- discarded test
         do continue giveUp
              st{ numDiscardedTests         = numDiscardedTests st + 1
                , numRecentlyDiscardedTests = numRecentlyDiscardedTests st + 1
                , randomSeed                = rnd2
                , expectedFailure           = expect
                } f

       MkResult{ok = Just False} -> -- failed test
         do if expect res
              then putPart (terminal st) (bold "*** Failed! ")
              else putPart (terminal st) "+++ OK, failed as expected. "
            (numShrinks, totFailed, lastFailed) <- foundFailure st res ts
            theOutput <- terminalOutput (terminal st)
            if not (expect res) then
              mkRes  Success{ labels = summary st,
                              numTests = numSuccessTests st+1,
                              output = theOutput }
             else
              mkRes  Failure{ usedSeed       = randomSeed st -- correct! (this will be split first)
                            , usedSize       = size
                            , numTests       = numSuccessTests st+1
                            , numShrinks     = numShrinks
                            , numShrinkTries = totFailed
                            , numShrinkFinal = lastFailed
                            , output         = theOutput
                            , reason         = P.reason res
                            , theException   = P.theException res
                            , labels         = summary st
                            }
 where
  (rnd1,rnd2) = split (randomSeed st)

summary :: State -> [(String,Int)]
summary st = reverse
           . sort
           . map (\ss -> (head ss, (length ss * 100) `div` numSuccessTests st))
           . group
           . sort
           $ [ concat (intersperse ", " s')
             | s <- collected st
             , let s' = [ t | (t,_) <- s ]
             , not (null s')
             ]

success :: State -> IO ()
success st =
  case allLabels ++ covers of
    []    -> do putLine (terminal st) "."
    [pt]  -> do putLine (terminal st)
                  ( " ("
                 ++ dropWhile isSpace pt
                 ++ ")."
                  )
    cases -> do putLine (terminal st) ":"
                sequence_ [ putLine (terminal st) pt | pt <- cases ]
 where
  allLabels = reverse
            . sort
            . map (\ss -> (showP ((length ss * 100) `div` numSuccessTests st) ++ head ss))
            . group
            . sort
            $ [ concat (intersperse ", " s')
              | s <- collected st
              , let s' = [ t | (t,0) <- s ]
              , not (null s')
              ]

  covers = [ ("only " ++ show occurP ++ "% " ++ fst (head lps) ++ "; not " ++ show reqP ++ "%")
           | lps <- groupBy first
                  . sort
                  $ [ lp
                    | lps <- collected st
                    , lp <- maxi lps
                    , snd lp > 0
                    ]
           , let occurP = (100 * length lps) `div` maxSuccessTests st
                 reqP   = maximum (map snd lps)
           , occurP < reqP
           ]

  (x,_) `first` (y,_) = x == y

  maxi = map (\lps -> (fst (head lps), maximum (map snd lps)))
       . groupBy first
       . sort

  showP p = (if p < 10 then " " else "") ++ show p ++ "% "

--------------------------------------------------------------------------
-- main shrinking loop

foundFailure :: State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
foundFailure st res ts =
  do localMin st{ numTryShrinks = 0 } res res ts

localMin :: State -> P.Result -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
localMin st MkResult{P.theException = Just e} lastRes _
  | isInterrupt e = localMinFound st lastRes
localMin st res _ ts = do
  putTemp (terminal st)
    ( short 26 (oneLine (P.reason res))
   ++ " (after " ++ number (numSuccessTests st+1) "test"
   ++ concat [ " and "
            ++ show (numSuccessShrinks st)
            ++ concat [ "." ++ show (numTryShrinks st) | numTryShrinks st > 0 ]
            ++ " shrink"
            ++ (if numSuccessShrinks st == 1
                && numTryShrinks st == 0
                then "" else "s")
             | numSuccessShrinks st > 0 || numTryShrinks st > 0
             ]
   ++ ")..."
    )
  r <- tryEvaluate ts
  case r of
    Left err ->
      localMinFound st
         (exception "Exception while generating shrink-list" err) { callbacks = callbacks res }
    Right ts' -> localMin' st res ts'

localMin' :: State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
localMin' st res [] = localMinFound st res
localMin' st res (t:ts) =
  do -- CALLBACK before_test
    MkRose res' ts' <- protectRose (reduceRose t)
    callbackPostTest st res'
    if ok res' == Just False
      then localMin st{ numSuccessShrinks = numSuccessShrinks st + 1,
                        numTryShrinks     = 0 } res' res ts'
      else localMin st{ numTryShrinks    = numTryShrinks st + 1,
                        numTotTryShrinks = numTotTryShrinks st + 1 } res res ts

localMinFound :: State -> P.Result -> IO (Int, Int, Int)
localMinFound st res =
  do let report = concat [
           "(after " ++ number (numSuccessTests st+1) "test",
           concat [ " and " ++ number (numSuccessShrinks st) "shrink"
                  | numSuccessShrinks st > 0
                  ],
           "): "
           ]
     if isOneLine (P.reason res)
       then putLine (terminal st) (P.reason res ++ " " ++ report)
       else do
         putLine (terminal st) report
         sequence_
           [ putLine (terminal st) msg
           | msg <- lines (P.reason res)
           ]
     callbackPostFinalFailure st res
     return (numSuccessShrinks st, numTotTryShrinks st - numTryShrinks st, numTryShrinks st)

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: State -> P.Result -> IO ()
callbackPostTest st res =
  sequence_ [ safely st (f st res) | PostTest _ f <- callbacks res ]

callbackPostFinalFailure :: State -> P.Result -> IO ()
callbackPostFinalFailure st res =
  sequence_ [ safely st (f st res) | PostFinalFailure _ f <- callbacks res ]

safely :: State -> IO () -> IO ()
safely st x = do
  r <- tryEvaluateIO x
  case r of
    Left e ->
      putLine (terminal st)
        ("*** Exception in callback: " ++ show e)
    Right x' ->
      return x'

--------------------------------------------------------------------------
-- the end.
