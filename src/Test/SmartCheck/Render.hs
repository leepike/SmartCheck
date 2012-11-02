-- | Rendering arbitrary data, and filling in holes in the data with variables.

module Test.SmartCheck.Render 
  ( renderWithVars
  , smartPrtLn
  , Replace(..)
  ) where

import Test.SmartCheck.Types hiding (format)
import Test.SmartCheck.DataToTree

import Data.Maybe
import Data.Tree
import Data.List
import Data.Char
import Control.Monad

---------------------------------------------------------------------------------

smartPrefix :: String
smartPrefix = "*** "

smartPrtLn :: String -> IO ()
smartPrtLn = putStrLn . (smartPrefix ++)

---------------------------------------------------------------------------------

-- | We track indicies/strings, etc. for values (subterms) and constructors
-- separately.
data Replace a = Replace { unVals :: [a], unConstrs :: [a] }
  deriving (Show, Read, Eq)

---------------------------------------------------------------------------------

-- XXX only print if variable list is non-empty.
renderWithVars :: SubTypes a => Format -> a -> Replace Idx -> IO ()
renderWithVars format d idxs = do
  prtVars "values" valsLen valVars
  prtVars "constructors" constrsLen constrVars
  constrArgs
  putStrLn ""
  putStrLn $ replaceWithVars format d idxs' (Replace valVars constrVars)
  putStrLn ""

  where
  idxs' = let cs = unConstrs idxs \\ unVals idxs in
          idxs { unConstrs = cs }

  constrArgs = 
    unless (constrsLen == 0) $ putStrLn "  there exist arguments x̅ s.t."

  prtVars kind len vs = 
    when (len > 0) $ 
         (putStrLn $ "forall " ++ kind ++ " "
      ++ unwords (take len vs) ++ ":")

  vars str   = map (\(x,i) -> x ++ show i) (zip (repeat str) [0::Integer ..])
  valVars    = vars "x"
  constrVars = vars "C"

  valsLen    = length (unVals idxs')
  constrsLen = length (unConstrs idxs')

---------------------------------------------------------------------------------

type VarRepl = Either String String

-- | At each index into d from idxs, replace the whole with a fresh value.
replaceWithVars :: SubTypes a
                => Format -> a -> Replace Idx -> Replace String -> String
replaceWithVars format d idxs vars = 
  case format of
    PrintTree   -> drawTree strTree
    -- We have to be careful here.  We can't just show d and then find the
    -- matching substrings to replace, since the same substring may show up in
    -- multiple places.  Rather, we have to recursively descend down the tree of
    -- substrings, finding matches, til we hit our variable.
    PrintString -> stitchTree strTree

  where
  strTree :: Tree String
  strTree = remSubVars (foldl' f t zipRepl)

    where
    -- Now we'll remove everything after the initial Rights, which are below
    -- variables.
    remSubVars (Node (Left s ) sf) = Node s (map remSubVars sf)
    remSubVars (Node (Right s) _ ) = Node s []

  f :: Tree VarRepl -> (String, Idx) -> Tree VarRepl
  f tree (var, idx) = Node (rootLabel tree) $
    case getIdxForest sf idx of
      Nothing                 -> errorMsg "replaceWithVars"
      Just (Node (Right _) _) -> sf -- Don't replace anything
      Just (Node (Left  _) _) -> forestReplaceChildren sf idx (Right var)

    where
    sf = subForest tree
  
  -- A tree representation of the data turned into a tree of Strings showing the
  -- data.  showForest is one of our generic methods.
  t :: Tree VarRepl
  t = let forest = showForest d in 
      if null forest then errorMsg "replaceWithVars" 
         else fmap Left (head forest) -- Should be a singleton

  -- Note: we put value idxs before constrs, since they take precedence.
  zipRepl :: [(String, Idx)]
  zipRepl =    zip (unVals vars)    (unVals idxs) 
            ++ zip (unConstrs vars) (unConstrs idxs)

---------------------------------------------------------------------------------

-- | Make a string out a Tree of Strings.  Put parentheses around complex
-- subterms, where "complex" means we have two or more items (i.e., there's a
-- space).
stitchTree :: Tree String -> String
stitchTree = stitch
  where 
  stitch (Node str forest) = str ++ " " ++ (unwords $ map stitchTree' forest)

  stitchTree' (Node str []) = if isJust $ find isSpace str 
                                then '(' : str ++ ")"
                                else str
  stitchTree' node = '(' : stitch node ++ ")"

---------------------------------------------------------------------------------
