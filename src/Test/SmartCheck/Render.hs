module Test.SmartCheck.Render 
  ( renderWithVars
  , smartPrtLn
  ) where

import Test.SmartCheck.Types
import Test.SmartCheck.DataToTree

import Data.Tree
import Data.Data
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
  t    = mkShowTree d
  vis  = zip vars idxs

  f :: Tree String -> (String, Idx) -> Tree String
  f tree (var, idx) = 
    let forest = forestReplaceChop (subForest tree) idx var in
    Node (rootLabel tree) forest

---------------------------------------------------------------------------------

-- | Make a string out a Tree of Strings.  Put parentheses around subterms.
stitchTree :: Tree String -> String
stitchTree = stitch
  where 
  stitchTree' (Node str []) = str
  stitchTree' node = '(' : stitch node ++ ")"

  stitch (Node str forest) = str ++ " " ++ (unwords $ map stitchTree' forest)

---------------------------------------------------------------------------------

-- Make a Tree out of a Data value.  On each level, we just use the user-defined
-- Show instance.  This is good in that it's what user expects, but it's bad in
-- that we show the entire subtree at each level.  
--
-- XXX Also, it's inconsistent since toConstr is not part of the user-defined
-- show instances.
mkShowTree :: SubTypes a => a -> Tree String
mkShowTree d = Node (show $ toConstr d) (strForest $ allSubTypes d)

strForest :: Forest SubT -> Forest String
strForest = map prtTree
  where
  prtTree (Node r forest) =
    let nextLevel       = map (show . rootLabel) forest in
    let stripSubForests = dropWhileEnd isSpace $
                          foldl' nubSubForest (show r) nextLevel in
    Node stripSubForests (strForest forest)
 
  -- Strips a subforest, including possible parentheses enclosing the
  -- expression.  Strip trailing whitespace when done.
  nubSubForest :: String -> String -> String
  nubSubForest str subTree = 
    go [] str

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
