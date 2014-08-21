{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

 -- Note: Benjamin Pierce's lecture notes are where I learned to 
 -- generate properly ordered binary search trees:

 -- http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/BST.html

module RedBlackSetTest where

import RedBlackSet 
import Test.QuickCheck
import System.Random (Random)
import Control.Monad
import Data.List (foldr)

import Test.SmartCheck
import Test.SmartCheck.Types
import GHC.Generics
import Data.Typeable


 -- Base-2 logarithm:
lg :: Int -> Int
lg n = round(logBase (fromIntegral(2)) (fromIntegral(n)))


 -- Generate unbounded trees:
unboundedTree :: Arbitrary a => Gen (RBSet a)
unboundedTree = 
 oneof [return E, 
        liftM4 T (oneof [return R,return B])
        unboundedTree arbitrary unboundedTree]


 -- Generate arbitrary trees:
boundedTree :: Arbitrary a => Gen (RBSet a)
boundedTree = sized tree where

 tree :: Arbitrary a => Int -> Gen (RBSet a)
 tree 0 = return E
 tree n | n>0 = 
   oneof [return E,
          liftM4 T color subtree arbitrary subtree]
   where subtree = tree (n `div` 2)
         color = oneof [return R, return B]


 -- Generate trees with no red-red violations:
nrrTree :: Arbitrary a => Gen (RBSet a)
nrrTree = sized (tree R) where

 tree :: Arbitrary a => Color -> Int -> Gen (RBSet a)

  -- Assuming black parent:
 tree B 0 = return E
 tree B n | n>0 = 
   oneof [return E,
          liftM4 T (return B) subtree  arbitrary subtree,
          liftM4 T (return R) subtree' arbitrary subtree']
   where subtree  = tree B (n `div` 2)
         subtree' = tree R (n `div` 2)

  -- Assuming red parent:
 tree R 0 = return E
 tree R n | n>0 = 
   oneof [return E,
          liftM4 T (return B) subtree arbitrary subtree]
   where subtree = tree B (n `div` 2)
        

 -- Generate black-balanced trees with no red-red violations:
balnrrTree :: Arbitrary a => Gen (RBSet a)
balnrrTree = sized (\n -> tree B (lg(n))) where

 tree :: Arbitrary a => Color -> Int -> Gen (RBSet a)

 tree B 0 = return E
 tree B 1 = 
   oneof [return E,
          liftM4 T (return R) (return E) arbitrary (return E)]

 tree B n | n>0 = 
   oneof [liftM4 T (return B) subtree arbitrary subtree,
          liftM4 T (return R) subtree' arbitrary subtree']
   where subtree  = tree B (n-1)
         subtree' = tree R n

 tree R 0 = return E
 tree R 1 = return E
 tree R n | n>0 = 
   oneof [liftM4 T (return B) subtree arbitrary subtree]
   where subtree = tree B (n-1)


 -- Generate ordered, black-balanced trees with no red-red violations:
ordbalnrrTree :: (Arbitrary a, Random a, 
                  Bounded a, Ord a, Num a) => Gen (RBSet a)
ordbalnrrTree =
-- sized (\n -> tree 0 100000000000000 B (lg n)) where
 sized (\n -> tree 0 2 B (lg n)) where

 tree min max _ _ | max < min = error $ "cannot generate"
 tree min max B 0 = return E
 tree min max B 1 =
   oneof [return E,
          liftM4 T (return R) (return E) (choose(min+1,max-1)) (return E)]
 tree min max B n | n>0 = 
   do key <- choose (min+1,max-1) 
      let subtree1 =  tree min (key-1) B (n-1)
      let subtree2 =  tree (key+1) max B (n-1)
      let subtree1' = tree min (key-1) R n
      let subtree2' = tree (key+1) max R n
      oneof [liftM4 T (return B) subtree1  (return key) subtree2,
             liftM4 T (return R) subtree1' (return key) subtree2']

 tree min max R 0 = return E
 tree min max R 1 = return E
 tree min max R n | n>0 = 
   do key <- choose (min+1, max-1)
      let subtree1 = tree min (key-1) B (n-1)
      let subtree2 = tree (key+1) max B (n-1)
      oneof [liftM4 T (return B) subtree1 (return key) subtree2]

 -- tree min max _ _ | max < min = error $ "cannot generate"
 -- tree min max B 0 = return E
 -- tree min max B 1 =
 --   oneof [return E,
 --          liftM4 T (return R) (return E) (choose(min,max)) (return E)]
 -- tree min max B n | n>0 = 
 --   do key <- choose (min,max) 
 --      let subtree1 =  tree min (key-1) B (n-1)
 --      let subtree2 =  tree (key+1) max B (n-1)
 --      let subtree1' = tree min (key-1) R n
 --      let subtree2' = tree (key+1) max R n
 --      oneof [liftM4 T (return B) subtree1  (return key) subtree2,
 --             liftM4 T (return R) subtree1' (return key) subtree2']

 -- tree min max R 0 = return E
 -- tree min max R 1 = return E
 -- tree min max R n | n>0 = 
 --   do key <- choose (min, max)
 --      let subtree1 = tree min (key-1) B (n-1)
 --      let subtree2 = tree (key+1) max B (n-1)
 --      oneof [liftM4 T (return B) subtree1 (return key) subtree2]


 -- Generate trees from insertions:
insertedTree :: (Arbitrary a, Ord a) => Gen (RBSet a)
insertedTree = liftM (Data.List.foldr insert empty) arbitrary

instance (Arbitrary a, Random a, 
          Bounded a, Ord a, Num a) => Arbitrary (RBSet a) where
  arbitrary = nrrTree
  -- arbitrary = oneof[ordbalnrrTree,
  --                   liftM (Data.List.foldr insert empty) arbitrary]

 -- Count the black depth of a red-black tree:
-- blackDepth :: RBSet a -> Maybe Int
-- blackDepth (E) = Just(1)
-- blackDepth (T R l _ r) = case (blackDepth(l),blackDepth(r)) of
--   (Just(n),Just(m)) -> if n == m then Just(n) else Nothing
--   (_,_) -> Nothing
-- blackDepth (T B l _ r) = case (blackDepth(l),blackDepth(r)) of
--   (Just(n),Just(m)) -> if n == m then Just(1+n) else Nothing
--   (_,_) -> Nothing

-- Bad
blackDepth :: RBSet a -> Maybe Int
blackDepth (E) = Just(1)
blackDepth (T R l _ r) = case (blackDepth(l),blackDepth(r)) of
  (Just(n),Just(m)) -> if n == m then Just(n) else Nothing
  (_,_) -> Nothing
blackDepth (T B l _ r) = case (blackDepth(l),blackDepth(r)) of
  (Just(n),Just(m)) -> if n == m then Just(1+n) else Nothing
  (_,_) -> Nothing

 -- Check for red-red violations:
prop_NoRedRed :: RBSet Int -> Bool
prop_NoRedRed E = True
prop_NoRedRed (T R (T R _ _ _) _ _) = False
prop_NoRedRed (T R _ _ (T R _ _ _)) = False
prop_NoRedRed (T _ l x r) = (prop_NoRedRed l) && (prop_NoRedRed r)


 -- Check for black-balanced violations:
prop_BlackBalanced :: RBSet Int -> Bool
prop_BlackBalanced t =
 case blackDepth(t) of
  Just _ -> True
  Nothing -> False


 -- Check for ordering violations:
prop_OrderedList :: Ord a => [a] -> Bool
prop_OrderedList [] = True
prop_OrderedList [x] = True
prop_OrderedList (x:y:tl) = (x < y) && (prop_OrderedList(y:tl))

prop_Ordered :: RBSet Int -> Bool
prop_Ordered t = prop_OrderedList (toAscList t) 

 -- Check for the validity of a red-black tree:
prop_RBValid :: RBSet Int -> Bool
prop_RBValid t = prop_NoRedRed t && prop_BlackBalanced t && prop_Ordered t


 -- Insertion properties:
prop_Create5 :: Int -> Int -> Int -> Int -> Int -> Bool
prop_Create5 a b c d e = 
  ((foldr insert empty) [a,b,c,d,e]) == 
  ((foldr insert empty) [b,c,d,e,a])

prop_InsertValid :: RBSet Int -> Int -> Bool
prop_InsertValid t x = prop_RBValid(insert x t)

prop_InsertMember :: RBSet Int -> Int -> Bool
prop_InsertMember t x = member x (insert x t)

prop_InsertSafe :: RBSet Int -> Int -> Int -> Property
prop_InsertSafe t x y = member x t ==> (member x (insert y t))

prop_NoInsertPhantom :: RBSet Int -> Int -> Int -> Property
prop_NoInsertPhantom t x y = 
 not (member x t) && x /= y ==> not (member x (insert y t))

 -- Deletion properties:
prop_InsertDeleteValid :: RBSet Int -> Int -> Bool
prop_InsertDeleteValid t x = prop_RBValid(delete x (insert x t))

prop_DeleteValid :: RBSet Int -> Int -> Bool
prop_DeleteValid t x = prop_RBValid(delete x t)

prop_MemberDelete :: RBSet Int -> Int -> Property
prop_MemberDelete t x = member x t ==> not (member x (delete x t))

prop_DeletePreserve :: RBSet Int -> Int -> Int -> Property
prop_DeletePreserve t x y = x /= y ==> (member y t) == (member y (delete x t))

test :: IO ()
test = 
 do quickCheck(prop_RBValid)

     -- Insertion tests:
    quickCheck(prop_Create5)
    quickCheck(prop_InsertValid)
    quickCheck(prop_InsertSafe)
    quickCheck(prop_NoInsertPhantom)
    quickCheck(prop_InsertMember)

     -- Deletion tests:
    quickCheck(prop_InsertDeleteValid)
    quickCheck(prop_DeleteValid)
    quickCheck(prop_MemberDelete)
    quickCheck(prop_DeletePreserve)

main :: IO ()
main = test

------------------------------------------------------------
-- SmartCheck

instance Arbitrary Color where
  arbitrary = elements [R, B, BB, NB]

instance SubTypes Color where
  baseType _    = True

instance ( Arbitrary a, Random a
         , Bounded a, Ord a, Num a
         , SubTypes a
         ) => SubTypes (RBSet a)

testSC :: IO ()
testSC = sc prop_BlackBalanced
  where
  sc = smartCheck scStdArgs {format = PrintString }

