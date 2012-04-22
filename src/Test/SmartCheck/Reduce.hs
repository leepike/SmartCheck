{-# LANGUAGE ScopedTypeVariables #-} 

module Test.SmartCheck.Reduce
  where

import Test.SmartCheck.Types
import Test.SmartCheck.Common
import Test.SmartCheck.DataToTree

import qualified Test.QuickCheck as Q
import Data.Maybe
import Data.Tree

---------------------------------------------------------------------------------

-- Smarter than shrinks.  Does substitution.  m is a value that failed QC that's
-- been shrunk.  We substitute successive children with strictly smaller (and
-- increasingly larger) randomly-generated values until we find a failure, and
-- return that result.  (We call smartShrink recursively.)
smartRun :: (Read a, Show a, Q.Arbitrary a, SubTypes a)
         => Q.Args -> (a -> Q.Property) -> IO (Maybe a)
smartRun args prop = do
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- runQC args genProp
  if (isJust res) then runSmart (fromJust res)
    else do putStrLn ""
            smartPrtLn "No value to smart-shrink; done."
            return Nothing

  where
  runSmart r = do
    putStrLn ""
    smartPrtLn "Smart Shrinking ... "
    new <- smartShrink args r prop
    smartPrtLn "Smart-shrunk value:"
    print new
    return (Just new)

---------------------------------------------------------------------------------

runQC :: forall a b. (Q.Testable b, Read a) => Q.Args -> Q.Gen b -> IO (Maybe a)
runQC args propGen = do
  res <- Q.quickCheckWithResult args propGen
  case res of
    -- XXX C'mon, QuickCheck, let me grab the result in a sane way rather than
    -- parsing a string!
    Q.Failure _ _ _ _ _ _ out -> do let ms = (lines out) !! 1 
                                    let m = (read ms) :: a
                                    return $ Just m
    _ -> return Nothing

---------------------------------------------------------------------------------
  
-- | Breadth-first traversal of d, trying to shrink it with *strictly* smaller
-- children.  We replace d whenever a successful shrink is found and try again.
smartShrink :: SubTypes a
            => Q.Args -> a -> (a -> Q.Property) -> IO a
smartShrink args d prop = iter d (Idx 0 0) 

  where 
  iter d' idx = do 
    putStrLn ("IDX: " ++ show idx)
    if done then return d'
      else if nextLevel 
             then iter d' (idx { column = 0
                               , level  = level idx + 1 
                               })
             else do if isNothing maxSize 
                       then iter d' (idx { column = column idx + 1 }) 
                       -- XXX We could shrink base values, but I'm not sure if
                       -- it's worth it.  Doesn't affect extrapolation or make
                       -- counter-examples more readable.
                       -- then case getAtIdx d' idx of
                       --        Nothing -> iter d' (idx { column = column idx + 1 }) 
                       --        Just v  -> mkVals v
                       else mkTry
                                  
    where
    mkTry = do try <- iterateArb d' idx (Q.maxDiscard args) 
                        (fromJust maxSize) notProp
               case try of
                 -- Found a try fails prop.  We'll now test try, and start
                 -- trying to reduce from the top!
                 Result x -> iter x (Idx 0 0)
                 -- Either couldn't satisfy the precondition or nothing
                 -- satisfied the property.  Either way, we can't shrink it.
                 _        -> iter d' (idx { column = column idx + 1 })

    forest    = mkSubstForest d'
    notProp   = Q.expectFailure . prop
                   

    -- XXX How do I know that the size of arbitrary relates to the depth of the
    -- structure?  However, things seem to work, but I'm not sure if it's
    -- because of the instances I defined.
    maxSize   = case getIdxForest forest idx of
                  Nothing -> Nothing
                  Just t  -> let dep = depth (subForest t) in
                             if dep <= 1 then Nothing
                               else Just (dep-1)

    pts       = breadthLevels forest
    done      = length pts <= level idx
    nextLevel = length (pts !! level idx) <= column idx

---------------------------------------------------------------------------------
