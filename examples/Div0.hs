{-# LANGUAGE DeriveDataTypeable #-}

-- | Divide by 0 example in a simple arithmetic language.

module Div0 where

import Test.QuickCheck 
import Test.SmartCheck
import Control.Monad
import Data.Data

data M = C Int
       | A M M
       | D M M
  deriving (Read, Show, Data, Typeable, Eq)

mkTypes :: M -> M -> Forest SubT
mkTypes m0 m1 = [ Node (subT m0) (subTypes m0)
                , Node (subT m1) (subTypes m1)
                ]

instance SubTypes M where
  subTypes (C _)     = []
  subTypes (A m0 m1) = mkTypes m0 m1 
  subTypes (D m0 m1) = mkTypes m0 m1 

eval :: M -> Maybe Int
eval (C i) = Just i
eval (A a b) = do
  i <- eval a 
  j <- eval b
  return $ i + j
eval (D a b) = 
  if eval b == Just 0 then Nothing 
    else do i <- eval a 
            j <- eval b
            return $ i `div` j

instance Arbitrary M where
  arbitrary = sized mkM
    where
    mkM 0 = liftM C arbitrary
    mkM n = oneof [ liftM2 A mkM' mkM' 
                  , liftM2 D mkM' mkM' ]
      where mkM' = mkM =<< choose (0,n-1)

  shrink (C _)   = []
  shrink (A a b) = [a, b]
  shrink (D a b) = [a, b]

div0 :: M -> Bool
div0 m = eval m /= Nothing

div1 :: M -> Property
div1 m =
--  True ==> 
  pre ==> eval m /= Nothing
  where
  -- precondition: no dividand in a subterm can evaluate to 0.
  pre = and $ map non0Divsors (divSubTerms m)
  non0Divsors m0 = eval m0 /= Just 0
  divSubTerms (C _)     = []
  divSubTerms (A m0 m1) = divSubTerms m0 ++ divSubTerms m1
  divSubTerms (D m0 m1) = m1 : divSubTerms m0 ++ divSubTerms m1
           
-- divProp :: Gen Prop
-- divProp = forAllShrink arbitrary shrink div1


main :: IO ()
main = do result <- smartRun args div1
          extrapolate args result div1
  where args = stdSmartArgs { qcArgs = stdArgs { maxSuccess = 1000
                                               , maxSize    = 20   }
                                               
                            }

---------------------------------------------------------------------------------
-- XXX cruft

-- Fails if some leaf in a complex m == (C 0)
prop0 :: M -> Property
prop0 m = 
  (complex m) 
--    False
    ==> f m
  where
  f (C i)     = i /= 0
  f (A m0 m1) = f m0 && f m1
  f (D m0 m1) = f m0 && f m1

  complex (C _) = False
  complex _     = True

-- Negation of a property.  
propNot :: (M -> Property) -> (M -> Property)
propNot prop = \m -> expectFailure (prop m)
--  p <- prop


-- props :: IO ()
-- props = do
--   -- 0 not valid, 1 succeed, 2 fail
--   let xs = [(C 0), A (C 2) (C 1), A (C 0) (C 1)]
--   let xs' = map prop0 xs :: [Gen Prop]
--   let fs = map (\(MkGen {unGen = f}) -> unProp $ f undefined undefined) xs' 
--              :: [Rose Result]
--   r <- mapM (protectRose . reduceRose) fs 
--          :: IO [Rose Result]
--   let reses = map (\(MkRose res _) -> res) r
--   putStrLn (show $ map ok reses)





-- runATest :: State -> (StdGen -> Int -> Prop) -> IO Result
-- runATest st f =
--   do let size = computeSize st (numSuccessTests st) (numDiscardedTests st)
--      MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
--      callbackPostTest st res
     
--      case res of
--        MkResult{ok = Just True, stamp = stamp, expect = expect} -> -- successful test
--          do test st{ numSuccessTests = numSuccessTests st + 1
--                    , randomSeed      = rnd2
--                    , collected       = stamp : collected st
--                    , expectedFailure = expect
--                    } f
       
--        MkResult{ok = Nothing, expect = expect} -> -- discarded test
--          do test st{ numDiscardedTests = numDiscardedTests st + 1
--                    , randomSeed        = rnd2
--                    , expectedFailure   = expect
--                    } f
         
--        MkResult{ok = Just False} -> -- failed test
--          do if expect res
--               then putPart (terminal st) (bold "*** Failed! ")
--               else putPart (terminal st) "+++ OK, failed as expected. "
--             numShrinks <- foundFailure st res ts
--             theOutput <- terminalOutput (terminal st)
--             if not (expect res) then
--               return Success{ labels = summary st,
--                               numTests = numSuccessTests st+1,
--                               output = theOutput }
--              else
--               return Failure{ usedSeed    = randomSeed st -- correct! (this will be split first)
--                             , usedSize    = size
--                             , numTests    = numSuccessTests st+1
--                             , numShrinks  = numShrinks
--                             , output      = theOutput
--                             , reason      = P.reason res
--                             , labels      = summary st
--                             }
--  where
--   (rnd1,rnd2) = split (randomSeed st)


