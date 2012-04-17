{-# LANGUAGE ScopedTypeVariables #-} 

module Test.SmartCheck.Reduce
  where

import Test.SmartCheck.Types
import Test.SmartCheck.Common
import Test.SmartCheck.DataToTree

import qualified Test.QuickCheck as Q
import Control.Monad
import Data.Maybe
import Data.Tree
--import Data.List

---------------------------------------------------------------------------------

-- Smarter than shrinks.  Does substitution.  m is a value that failed QC that's
-- been shrunk.  We substitute successive children with strictly smaller (and
-- increasingly larger) randomly-generated values until we find a failure, and
-- return that result.  (We call smartShrink recursively.)
smartRun :: (Read a, Show a, Q.Arbitrary a, SubTypes a)
         => SmartArgs -> (a -> Q.Property) -> IO a
smartRun args prop = do
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- runQC (qcArgs args) genProp
  unless (isJust res) (return ())
  new <- smartShrink args (fromJust res) prop
  putStrLn ""
  putStrLn $ smartPrefix ++ "Smart Shrinking ... "
  putStrLn $ smartPrefix ++ "Smart-shrunk value:"
  print new
  return new

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
            => SmartArgs -> a -> (a -> Q.Property) -> IO a
smartShrink args d prop = iter d (Idx 0 0) 

  where 
  iter d' idx = do 
    if done then return d'
      else if nextLevel 
             then iter d' (idx { level = level idx + 1 })
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
    mkTry = do try <- iterateArb d' idx (shrinks args) 
                        (fromJust maxSize) notProp
               -- first failing try
               if isJust try
               -- Found a try fails prop.  We'll now test try, and start trying
               -- to reduce from the top!
                 then iter (fromJust try) (Idx 0 0)
               -- Can't generalize.
                 else iter d' (idx { column = column idx + 1 }) 

    forest    = mkSubstForest d'
--    notProp   = not . prop
    -- notProp   = do p <- prop
    --                return $ not p
    notProp   = undefined
                   

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

    -- mkVals SubT { unSubT = v } = 
    --   let vs = map (replaceAtIdx d' idx) (Q.shrink v) in
    --   case find notProp (catMaybes vs) of
    --     Nothing -> iter d' (idx { column = column idx + 1 }) 
    --     Just x  -> iter x (idx { column = column idx + 1 }) 

---------------------------------------------------------------------------------
