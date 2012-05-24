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
    PrntTree   -> drawTree strTree
    -- We have to be careful here.  We can't just show d and then find the
    -- matching substrings to replace, since the same substring may show up in
    -- multiple places.  Rather, we have to recursively descend down the tree of
    -- substrings, finding matches, til we hit our variable.
    PrntString -> stitchTree strTree

  where
  strTree = foldl' f t vis
  t    = Node (toConstr d) (strForest $ allSubTypes d)
  vis  = zip vars idxs

  f :: Tree String -> (String, Idx) -> Tree String
  f tree (var, idx) = 
    let forest = forestReplaceChop (subForest tree) idx var in
    Node (rootLabel tree) forest

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

strForest :: Forest SubT -> Forest String
strForest = map prtTree

prtTree :: Tree SubT -> Tree String
prtTree (Node r forest) =
  let nextLevel       = map (show . rootLabel) forest in
  -- XXX A hack!  May not work if showing a constructor has spaces.
  let stripSubForests = dropWhileEnd isSpace $
                        foldl' nubSubForest (show r) nextLevel in
  Node stripSubForests (strForest forest)
 
  where
  -- Strips a subforest, including possible parentheses enclosing the
  -- expression.  Strip trailing whitespace when done.
  nubSubForest :: String -> String -> String
  nubSubForest str subTree = go [] str

    where
    go acc [] = reverse acc
    go acc ss = 
      case stripPrefix inParens ss of
        Just rst -> reverse acc ++ rst
        Nothing  -> case stripPrefix subTree ss of
                      Nothing  -> go (head ss : acc) (tail ss)
                      Just rst -> reverse acc ++ rst

    inParens = '(' : subTree ++ ")"

---------------------------------------------------------------------------------
