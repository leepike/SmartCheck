{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE Rank2Types #-} 

-- | Interface module.

module Test.SmartCheck 
  ( smartCheck
  , module Test.SmartCheck.Types
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.Reduce
import Test.SmartCheck.Extrapolate
import Test.SmartCheck.Render
import Test.SmartCheck.ConstructorGen

import qualified Test.QuickCheck as Q

---------------------------------------------------------------------------------

-- | Main interface function.
smartCheck :: forall a. (Read a, Q.Arbitrary a, SubTypes a)
           => ScArgs -> (a -> Q.Property) -> IO ()
smartCheck args prop = smartCheck' prop []

  where
  smartCheck' prop' ds = do
    -- Run standard QuickCheck
    res <- runQC (qcArgs args) prop'

    -- Run the smart reduction algorithm
    d   <- smartRun args res prop
    
    -- If we asked to extrapolate values, do so.
    vs  <- if (extrap args) 
              then case d of
                     Nothing -> return Nothing --continue [] id ds
                     -- Extrapolate with the original property to see if we get
                     -- a previously-visited value back.
                     Just d' -> do (idxs, prop_) <- extrapolate args d' prop ds

                                   putStrLn $ "ex d: " ++ show d -- YYY
                                   putStrLn $ "ex idx " ++ show idxs -- YYY

                                   return $ Just (idxs, prop_, d' : ds)
                                   --continue idx prop_ (d' : ds)

              else return Nothing

    -- If we asked to extrapolate constructors, do so.
    cs  <- if (constrGen args)
             then case d of
                    Nothing -> return Nothing
                    Just d' -> constrsGen args d' >>= return . Just
             else return Nothing

    -- If either extraopolation pass yielded fruit, prettyprint it.
    if (nonEmpty vs cs)
      then case d of 
             -- We shouldn't have non-empty extrapolation if there was no
             -- successful shrink!
             Nothing -> errorMsg "smartCheck"  
             Just d' -> (output d' (repls vs cs))
      else smartPrtLn "Could not extrapolate a new value; done."

    -- Ask the user if she wants to try again.
    putStrLn $ "Attempt to find a new counterexample?" 
                 ++ " ('Enter' to continue;"
                 ++ " any character then 'Enter' to quit.)"
    s <- getLine
    if (s == "")
      then smartCheck' (f vs $ prop) (newVals vs)
      else smartPrtLn "Done."

    where
    
    newVals :: Maybe (Extrapolate a) -> [a]
    newVals vs = case vs of
                   Nothing          -> ds
                   Just (_, _, ds') -> ds' 

    output :: a -> Replace Idx -> IO ()
    output d repl = do
      putStrLn $ "d: " ++ show d -- YYY
      putStrLn $ "idx " ++ show repl -- YYY
      
      smartPrtLn "Extrapolated value:"
      renderWithVars (treeShow args) d repl -- XXX

  --------------------------------------

  repls :: Maybe (Extrapolate a) -> Maybe [Idx] -> Replace Idx
  repls vs cs = Replace v c
    where 
    v = case vs of 
          Nothing           -> []
          Just (idxs, _, _) -> idxs
    c = case cs of 
          Nothing    -> []
          Just idxs  -> idxs

  f vs = case vs of 
           Nothing            -> id
           Just (_, prop_, _) -> prop_


  nonEmpty vs cs = vsIdxs || csIdxs
    where
    vsIdxs = case vs of
               Just (idxs, _, _) -> not $ null idxs
               Nothing           -> False
    csIdxs = case cs of
               Just idxs -> not $ null idxs
               Nothing   -> False 

---------------------------------------------------------------------------------

runQC :: forall a. (Show a, Read a, Q.Arbitrary a)
      => Q.Args -> (a -> Q.Property) -> IO (Maybe a)
runQC args prop = do
  let genProp = Q.forAllShrink Q.arbitrary Q.shrink prop
  res <- Q.quickCheckWithResult args genProp
  case res of
    -- XXX C'mon, QuickCheck, let me grab the result in a sane way rather than
    -- parsing a string!
    Q.Failure _ _ _ _ _ _ out -> do let ms = (lines out) !! 1 
                                    let m = (read ms) :: a
                                    return $ Just m
    _ -> return Nothing

---------------------------------------------------------------------------------

type Extrapolate a = ( [Idx]
                     , ((a -> Q.Property) -> a -> Q.Property)
                     , [a]
                     )

---------------------------------------------------------------------------------
