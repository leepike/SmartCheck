-- | Rendering arbitrary data, and filling in holes in the data with variables.

module Test.SmartCheck.Render 
  ( renderWithVars
  , smartPrtLn
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree

import Data.Maybe
import Data.Tree
import Data.List
import Data.Char

---------------------------------------------------------------------------------

smartPrefix :: String
smartPrefix = "*** "

smartPrtLn :: String -> IO ()
smartPrtLn = putStrLn . (smartPrefix ++)

---------------------------------------------------------------------------------

renderWithVars :: SubTypes a => Format -> a -> [Idx] -> IO ()
renderWithVars format d idxs = do
  putStrLn $ "forall " ++ unwords (take (length idxs) vars) ++ ":"
  putStrLn ""
  putStrLn $ replaceWithVars format d idxs vars
  putStrLn ""
  where
  vars = map (\(x,i) -> x ++ show i) $ zip (repeat "x") [0::Int ..]

---------------------------------------------------------------------------------

-- | At each index into d from idxs, replace the whole with a fresh value.
replaceWithVars :: SubTypes a
                => Format -> a -> [Idx] -> [String] -> String
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
  strTree = foldl' f t (zip vars idxs)
  
  -- A tree representation of the data turned into a tree of Strings showing the
  -- data.  Note that just like in the representation, the a parent contains its
  -- children as substrings.
  t :: Tree String
  t = let forest = showForest d in
      if null forest then errorMsg "replaceWithVars" 
         else head forest

  f :: Tree String -> (String, Idx) -> Tree String
  f tree (var, idx) = 
    Node (rootLabel tree) (forestReplaceChop (subForest tree) idx var)

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
