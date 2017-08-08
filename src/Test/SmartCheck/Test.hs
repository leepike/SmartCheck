{-

The following is modified by Lee Pike (2014) and still retains the following
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
{-# LANGUAGE ExistentialQuantification #-}

-- | SmartCheck's interface to QuickCheck.

module Test.SmartCheck.Test
  ( scQuickCheckWithResult
  , stdArgs
  ) where

--------------------------------------------------------------------------
-- imports

import Prelude hiding (break)

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException), labels )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
import qualified Test.QuickCheck.State as S
import Test.QuickCheck.Exception
import Test.QuickCheck.Random
import System.Random (split)

import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , group
  , intersperse
  )

--------------------------------------------------------------------------
-- quickCheck

-- | Our SmartCheck reimplementation of the main QuickCheck driver.  We want to
-- distinguish the first argument to a 'Testable' property to be SmartChecked.
-- In particular: the first argument will not be shrunk (even if there are
-- default shrink instances for the type).  However, the argument will be grown
-- according to the the 'maxSize' argument to QuickCheck, in accordance with its
-- generator.  Other arguments will be shrunk, if they have shrinking instances.
scQuickCheckWithResult :: forall a prop. (Show a, Arbitrary a, Testable prop)
  => Args -> (a -> prop) -> IO (Maybe a, Result)
scQuickCheckWithResult a p = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case replay a of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     test S.MkState{ S.terminal                  = tm
                   , S.maxSuccessTests           = maxSuccess a
                   , S.maxDiscardedTests         = maxDiscardRatio a * maxSuccess a
                   , S.computeSize               = case replay a of
                                                   Nothing    -> computeSize'
                                                   Just (_,s) -> computeSize' `at0` s
                   , S.numSuccessTests           = 0
                   , S.numDiscardedTests         = 0
                   , S.labels                    = M.empty
                   , S.numRecentlyDiscardedTests = 0
                   , S.collected                 = []
                   , S.expectedFailure           = False
                   , S.randomSeed                = rnd
                   , S.numSuccessShrinks         = 0
                   , S.numTryShrinks             = 0
                   , S.numTotTryShrinks          = 0
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

test :: Arbitrary a => S.State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
test st f
  | S.numSuccessTests st   >= S.maxSuccessTests st   = doneTesting st f
  | S.numDiscardedTests st >= S.maxDiscardedTests st = giveUp st f
  | otherwise                                        = runATest st f

doneTesting :: S.State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
doneTesting st _f =
  do -- CALLBACK done_testing?
     if S.expectedFailure st then
       putPart (S.terminal st)
         ( "+++ OK, passed "
        ++ show (S.numSuccessTests st)
        ++ " tests"
         )
      else
       putPart (S.terminal st)
         ( bold ("*** Failed!")
        ++ " Passed "
        ++ show (S.numSuccessTests st)
        ++ " tests (expected failure)"
         )
     success st
     theOutput <- terminalOutput (S.terminal st)
     return $ (Nothing, if S.expectedFailure st then
                          Success{ labels = summary st,
                                   numTests = S.numSuccessTests st,
                                   output = theOutput }
                        else NoExpectedFailure{ labels = summary st,
                                                numTests = S.numSuccessTests st,
                                                output = theOutput })

giveUp :: S.State -> (QCGen -> Int -> (a -> Prop)) -> IO (Maybe a, Result)
giveUp st _f =
  do -- CALLBACK gave_up?
     putPart (S.terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (S.numSuccessTests st)
      ++ " tests"
       )
     success st
     theOutput <- terminalOutput (S.terminal st)
     return ( Nothing
            , GaveUp{ numTests = S.numSuccessTests st
                    , labels   = summary st
                    , output   = theOutput
                    }
            )

runATest :: forall a. (Arbitrary a)
         => S.State
         -> (QCGen -> Int -> (a -> Prop))
         -> IO (Maybe a, Result)
runATest st f =
  do -- CALLBACK before_test
     putTemp (S.terminal st)
        ( "("
       ++ number (S.numSuccessTests st) "test"
       ++ concat [ "; " ++ show (S.numDiscardedTests st) ++ " discarded"
                 | S.numDiscardedTests st > 0
                 ]
       ++ ")"
        )
     let size = S.computeSize st (S.numSuccessTests st) (S.numRecentlyDiscardedTests st)

     let p :: a -> Prop
         p = f rnd1 size

     let genA :: QCGen -> Int -> a
         genA = unGen arbitrary
     let rndA = genA rnd1 size

     let mkRes res = return (Just rndA, res)

     MkRose res ts <- protectRose (reduceRose (unProp (p rndA)))
     callbackPostTest st res

     let continue break st' | abort res = break st'
                            | otherwise = test st'

     case res of
       MkResult{ok = Just True, stamp, expect} -> -- successful test
         do continue doneTesting
              st{ S.numSuccessTests           = S.numSuccessTests st + 1
                , S.numRecentlyDiscardedTests = 0
                , S.randomSeed                = rnd2
                , S.collected                 = stamp : S.collected st
                , S.expectedFailure           = expect
                } f

       MkResult{ok = Nothing, expect = expect} -> -- discarded test
         do continue giveUp
              st{ S.numDiscardedTests         = S.numDiscardedTests st + 1
                , S.numRecentlyDiscardedTests = S.numRecentlyDiscardedTests st + 1
                , S.randomSeed                = rnd2
                , S.expectedFailure           = expect
                } f

       MkResult{ok = Just False} -> -- failed test
         do if expect res
              then putPart (S.terminal st) (bold "*** Failed! ")
              else putPart (S.terminal st) "+++ OK, failed as expected. "
            (numShrinks, totFailed, lastFailed) <- foundFailure st res ts
            theOutput <- terminalOutput (S.terminal st)
            if not (expect res) then
              mkRes  Success{ labels   = summary st,
                              numTests = S.numSuccessTests st+1,
                              output   = theOutput
                            }
             else
              mkRes  Failure{ -- correct! (this will be split first)
                              usedSeed       = S.randomSeed st
                            , usedSize       = size
                            , numTests       = S.numSuccessTests st+1
                            , numShrinks     = numShrinks
                            , numShrinkTries = totFailed
                            , numShrinkFinal = lastFailed
                            , output         = theOutput
                            , reason         = P.reason res
                            , theException   = P.theException res
                            , labels         = summary st
                            }
 where
  (rnd1,rnd2) = split (S.randomSeed st)

summary :: S.State -> [(String,Int)]
summary st = reverse
           . sort
           . map (\ss -> (head ss, (length ss * 100) `div` S.numSuccessTests st))
           . group
           . sort
           $ [ concat (intersperse ", " (Set.toList s))
             | s <- S.collected st
             , not (Set.null s)
             ]

success :: S.State -> IO ()
success st =
  case allLabels ++ covers of
    []    -> do putLine (S.terminal st) "."
    [pt]  -> do putLine (S.terminal st)
                  ( " ("
                 ++ dropWhile isSpace pt
                 ++ ")."
                  )
    cases -> do putLine (S.terminal st) ":"
                sequence_ [ putLine (S.terminal st) pt | pt <- cases ]
 where
  allLabels = reverse
            . sort
            . map (\ss -> (showP ((length ss * 100) `div` S.numSuccessTests st) ++ head ss))
            . group
            . sort
            $ [ concat (intersperse ", " s')
              | s <- S.collected st
              , let s' = [ t | t <- Set.toList s, M.lookup t (S.labels st) == Just 0 ]
              , not (null s')
              ]

  covers = [ ("only " ++ show (labelPercentage l st) ++ "% " ++ l ++ ", not " ++ show reqP ++ "%")
           | (l, reqP) <- M.toList (S.labels st)
           , labelPercentage l st < reqP
           ]

  -- (x,_) `first` (y,_) = x == y

  showP p = (if p < 10 then " " else "") ++ show p ++ "% "

labelPercentage :: String -> S.State -> Int
labelPercentage l st =
    -- XXX in case of a disjunction, a label can occur several times,
    -- need to think what to do there
    (100 * occur) `div` S.maxSuccessTests st
  where
        occur = length [ l' | l' <- concat (map Set.toList (S.collected st)), l == l' ]

--------------------------------------------------------------------------
-- main shrinking loop

foundFailure :: S.State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
foundFailure st res ts =
  do localMin st{ S.numTryShrinks = 0 } res res ts

localMin :: S.State -> P.Result -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
localMin st MkResult{P.theException = Just e} lastRes _
  | isInterrupt e = localMinFound st lastRes
localMin st res _ ts = do
  putTemp (S.terminal st)
    ( short 26 (oneLine (P.reason res))
   ++ " (after " ++ number (S.numSuccessTests st+1) "test"
   ++ concat [ " and "
            ++ show (S.numSuccessShrinks st)
            ++ concat [ "." ++ show (S.numTryShrinks st) | S.numTryShrinks st > 0 ]
            ++ " shrink"
            ++ (if S.numSuccessShrinks st == 1
                && S.numTryShrinks st == 0
                then "" else "s")
             | S.numSuccessShrinks st > 0 || S.numTryShrinks st > 0
             ]
   ++ ")..."
    )
  r <- tryEvaluate ts
  case r of
    Left err ->
      localMinFound st
         (exception "Exception while generating shrink-list" err) { callbacks = callbacks res }
    Right ts' -> localMin' st res ts'

localMin' :: S.State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int)
localMin' st res [] = localMinFound st res
localMin' st res (t:ts) =
  do -- CALLBACK before_test
    MkRose res' ts' <- protectRose (reduceRose t)
    callbackPostTest st res'
    if ok res' == Just False
      then localMin st{ S.numSuccessShrinks = S.numSuccessShrinks st + 1,
                        S.numTryShrinks     = 0 } res' res ts'
      else localMin st{ S.numTryShrinks    = S.numTryShrinks st + 1,
                        S.numTotTryShrinks = S.numTotTryShrinks st + 1 } res res ts

localMinFound :: S.State -> P.Result -> IO (Int, Int, Int)
localMinFound st res =
  do let report = concat [
           "(after " ++ number (S.numSuccessTests st+1) "test",
           concat [ " and " ++ number (S.numSuccessShrinks st) "shrink"
                  | S.numSuccessShrinks st > 0
                  ],
           "): "
           ]
     if isOneLine (P.reason res)
       then putLine (S.terminal st) (P.reason res ++ " " ++ report)
       else do
         putLine (S.terminal st) report
         sequence_
           [ putLine (S.terminal st) msg
           | msg <- lines (P.reason res)
           ]
     putLine (S.terminal st) "*** Non SmartChecked arguments:"

     callbackPostFinalFailure st res
     return (S.numSuccessShrinks st, S.numTotTryShrinks st - S.numTryShrinks st, S.numTryShrinks st)

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: S.State -> P.Result -> IO ()
callbackPostTest st res =
  sequence_ [ safely st (f st res) | PostTest _ f <- callbacks res ]

callbackPostFinalFailure :: S.State -> P.Result -> IO ()
callbackPostFinalFailure st res =
  sequence_ [ safely st (f st res) | PostFinalFailure _ f <- callbacks res ]

safely :: S.State -> IO () -> IO ()
safely st x = do
  r <- tryEvaluateIO x
  case r of
    Left e ->
      putLine (S.terminal st)
        ("*** Exception in callback: " ++ show e)
    Right x' ->
      return x'

--------------------------------------------------------------------------
-- the end.
