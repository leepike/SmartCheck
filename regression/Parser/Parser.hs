{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Toy "parser"/"serializer" (with a bug).

module Main where

import Prelude hiding (showList, mod)

#if defined(qc) || defined(qcGen) || defined(smart)
import Test
import System.Environment
#endif

import Test.QuickCheck
import Test.SmartCheck
import Data.List

import GHC.Generics
import Data.Typeable
import Control.Applicative

import Control.Monad.State
import Data.Char

#ifdef feat
import Test.Feat
#endif

-----------------------------------------------------------------

-- Let's make up a toy language.

data Lang = Lang
  { modules :: [Mod]
  , funcs   :: [Func]
  } deriving (Show, Read, Typeable, Generic, Eq)

instance SubTypes Lang

newtype Var = Var String
  deriving (Show, Read, Typeable, Generic, Eq)

instance SubTypes Var
  where baseType _ = True

data Mod = Mod
  { imports :: [Var]
  , exports :: [Var]
  } deriving (Show, Read, Typeable, Generic, Eq)

instance SubTypes Mod

data Func = Func
  { fnName :: Var
  , args   :: [Exp]
  , stmts  :: [Stmt]
  } deriving (Show, Read, Typeable, Generic, Eq)

instance SubTypes Func

data Stmt = Assign Var Exp
          | Alloc Var Exp
          | Return Exp
          -- | Ref Exp
          -- | Deref Exp
          -- | Assert Exp
          -- | Loop Exp [Stmt]
          -- | IfTE Exp [Stmt] [Stmt]
  deriving (Show, Read, Typeable, Generic, Eq)

instance SubTypes Stmt

data Exp = Int Int
         | Bool Bool
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Not Exp
         | And Exp Exp
         | Or Exp Exp
  deriving (Show, Read, Typeable, Generic, Eq)

instance SubTypes Exp

-- Feat --------------------------------
#ifdef feat
deriveEnumerable ''Lang
deriveEnumerable ''Var
deriveEnumerable ''Mod
deriveEnumerable ''Func
deriveEnumerable ''Stmt
deriveEnumerable ''Exp
#endif
-- Feat --------------------------------

--------------------------------------------------------------------------------

nonEmpty :: Gen [a] -> Gen [a]
nonEmpty a = suchThat a (not . null)

instance Arbitrary Var where
  arbitrary = Var <$> suchThat arbitrary
    (\s -> all isAlphaNum s && not (null s))

instance Arbitrary Lang where
  arbitrary = Lang <$> nonEmpty arbitrary <*> nonEmpty arbitrary
#ifdef qc
  shrink (Lang m f) = map go (shrink (m, f))
    where go (a,b) = Lang a b
#endif
#ifdef qcGen
  shrink = genericShrink
#endif

instance Arbitrary Mod where
  arbitrary = Mod <$> nonEmpty arbitrary <*> nonEmpty arbitrary
#ifdef qc
  shrink (Mod a b) = map go (shrink (a, b))
    where go (x,y) = Mod x y
#endif
#ifdef qcGen
  shrink = genericShrink
#endif

instance Arbitrary Func where
  arbitrary = Func <$> arbitrary <*> nonEmpty arbitrary <*> nonEmpty arbitrary
#ifdef qc
  shrink (Func f a st) = map go (shrink (a, st))
    where go (x, s) = Func f x s
#endif
#ifdef qcGen
  shrink = genericShrink
#endif

instance Arbitrary Stmt where
  arbitrary = do
    v  <- arbitrary
    e  <- arbitrary
    let a0 = Assign v e
    let a1 = Alloc v e
    let a2 = Return e
    elements [a0, a1, a2]
#ifdef qc
  shrink stmt = case stmt of
    Assign v e -> map (Assign v) (shrink e)
    Alloc v e  -> map (Alloc v) (shrink e)
    Return e   -> map Return (shrink e)
#endif
#ifdef qcGen
  shrink = genericShrink
#endif

instance Arbitrary Exp where
  arbitrary = do
    i <- Int  <$> arbitrary
    b <- Bool <$> arbitrary
    a <- Add  <$> g <*> g
    s <- Sub  <$> g <*> g
    m <- Mul  <$> g <*> g
    d <- Div  <$> g <*> g
    t <- Not  <$> g
    n <- And  <$> g <*> g
    o <- Or   <$> g <*> g
    elements [i, b, a, s, m, d, t, n, o]
    where
    igen = Int  <$> arbitrary
    bgen = Bool <$> arbitrary
    g :: Gen Exp
    g = go =<< choose (1::Int, 10)
    go 0 = oneof [igen, bgen]
    go i = do let goa = go =<< choose (0, i)
              let gob = go =<< choose (0, i)
              oneof [ Add  <$> goa <*> gob
                    , Sub  <$> goa <*> gob
                    , Mul  <$> goa <*> gob
                    , Div  <$> goa <*> gob
                    , Not  <$> goa
                    , And  <$> goa <*> gob
                    ]
#ifdef qc
  shrink e = case e of
    Int  i    -> map Int (shrink i)
    Bool b    -> map Bool (shrink b)
    Add e0 e1 -> map (uncurry Add) (zip (shrink e0) (shrink e1))
    Sub e0 e1 -> map (uncurry Sub) (zip (shrink e0) (shrink e1))
    Mul e0 e1 -> map (uncurry Mul) (zip (shrink e0) (shrink e1))
    Div e0 e1 -> map (uncurry Div) (zip (shrink e0) (shrink e1))
    Not e0    -> map Not (shrink e0)
    And e0 e1 -> map (uncurry And) (zip (shrink e0) (shrink e1))
    Or e0 e1  -> map (uncurry Or) (zip (shrink e0) (shrink e1))
#endif
#ifdef qcGen
  shrink = genericShrink
#endif

--------------------------------------------------------------------------------
-- "serializer"

parens :: String -> String
parens a = '(' : a ++ ")"

showList :: Show' a => Char -> [a] -> String
showList sep ls = parens $ concat $ intersperse [sep] $ map show' ls

class Show a => Show' a where
  show' :: a -> String
  show' = show

instance Show' Char
instance Show' Int
instance Show' Bool

instance Show' Lang where
  show' (Lang m f)   = unwords
    [ "Lang"
    , showList ';' m
    , showList ';' f
    ]

instance Show' Mod where
  show' (Mod i e)    = unwords
    [ "Mod"
    , showList ':' i
    , showList ':' e
    ]

instance Show' Func where
  show' (Func f a s) = unwords
   [ "Func"
   , show' f
   , showList ',' a
   , showList ',' s
   ]

instance Show' Var where
  show' (Var v) = v

instance Show' Stmt where
  show' stmt         = unwords $ case stmt of
    Assign v e -> ["Assign", show' v, parens $ show' e]
    Alloc v e  -> ["Alloc" , show' v, parens $ show' e]
    Return e   -> ["Return",          parens $ show' e]

instance Show' Exp where
  show' e = unwords $ case e of
    Int i     -> ["Int" , show' i]
    Bool b    -> ["Bool", show' b]
    Add e0 e1 -> ["Add" , parens $ show' e0, parens $ show' e1]
    Sub e0 e1 -> ["Sub" , parens $ show' e0, parens $ show' e1]
    Mul e0 e1 -> ["Mul" , parens $ show' e0, parens $ show' e1]
    Div e0 e1 -> ["Div" , parens $ show' e0, parens $ show' e1]
    Not e0    -> ["Not" , parens $ show' e0]
    And e0 e1 -> ["And" , parens $ show' e0, parens $ show' e1]
    Or  e0 e1 -> ["Or" , parens $ show' e0, parens $ show' e1]

--------------------------------------------------------------------------------
-- "parser"

class Read a => Read' a where
  read' :: String -> a
  read' = read

instance Read' Lang where
  read' str   = run str $ do
    modify (strip "Lang")
    m <- state unparens
    let ms = map read' (fromSeps ';' m)
    f <- state unparens
    let fs = map read' (fromSeps ';' f)
    return (Lang ms fs)

instance Read' Mod where
  read' mod    = run mod $ do
                   modify (strip "Mod")
                   m <- state unparens
                   let i = fromSeps ':' m
                   es <- state unparens
                   let e = fromSeps ':' es
                   return (Mod (map Var i) (map Var e))

instance Read' Func where
  read' f = run f $ do
              modify (strip "Func")
              n     <- state (procWord id)
              as    <- state unparens
              let ars = map read' (fromSeps ',' as)
              ss <- state unparens
              let sts = map read' (fromSeps ',' ss)
              return (Func (Var n) ars sts)

instance Read' Stmt where
  read' stmt | isPrefixOf "Assign" stmt = run stmt $ do
                                            modify (strip "Assign")
                                            v <- state (procWord id)
                                            e <- state (procParens read')
                                            return (Assign (Var v) e)
             | isPrefixOf "Alloc" stmt  = run stmt $ do
                                            modify (strip "Alloc")
                                            v <- state (procWord id)
                                            e <- state (procParens read')
                                            return (Alloc (Var v) e)
             | isPrefixOf "Return" stmt = run stmt $ do
                                            modify (strip "Return")
                                            e <- state (procParens read')
                                            return (Return e)
             | otherwise                = error $ "Couldn't match stmt " ++ stmt

instance Read' Exp where
  read' e | isPrefixOf "Int"  e = Int  (read $ strip "Int" e)
          | isPrefixOf "Bool" e = Bool (read $ strip "Bool" e)
          | isPrefixOf "Add"  e = run e $ do
                                    modify (strip "Add")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Add e0 e1)

          | isPrefixOf "Sub"  e = run e $ do
                                    modify (strip "Sub")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Sub e0 e1)

          | isPrefixOf "Mul"  e = run e $ do
                                    modify (strip "Mul")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Mul e0 e1)

          | isPrefixOf "Div"  e = run e $ do
                                    modify (strip "Div")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    return (Div e0 e1)

          | isPrefixOf "Not"  e = run e $ do
                                    modify (strip "Not")
                                    e0 <- state (procParens read')
                                    return (Not e0)

          | isPrefixOf "And"  e = run e $ do
                                    modify (strip "And")
                                    e0 <- state (procParens read')
                                    e1 <- state (procParens read')
                                    -- XXX Bug!
                                    return (And e1 e0)
          | otherwise           = error $ "Couldn't match exp " ++ e

--------------------------------------------------------------------------------

run :: s -> State s a -> a
run e m = (flip evalState) e m

-- strip a prefix and a space from a string.  Return the remainder of the
-- string.
strip :: String -> String -> String
strip pre str = case stripPrefix pre str of
  Nothing   -> error $ "Couldn't strip " ++ pre ++ " from " ++ str
  Just rst  -> if null rst then rst else tail rst

-- Strip the next word.
stripWord :: String -> (String, String)
stripWord str = let strs = words str in
                (head strs, unwords (tail strs))


procWord :: (String -> a) -> String -> (a, String)
procWord = runProc stripWord

-- Return a prefix inside parens and the remainder of a string.
unparens :: String -> (String, String)
unparens ('(':str) = unparens' (1::Integer) [] str
  where
  unparens' n s ('(':r) = unparens' (n+1) ('(':s) r
  unparens' n s (')':r) | n == 1    = (reverse s, strip "" r)
                        | otherwise = unparens' (n-1) (')':s) r
  unparens' _ _ []      = error $ "End of string reached in unparens"
  unparens' n s (c:r)   = unparens' n (c:s) r
unparens str = error $ "Unparsens couldn't parse " ++ str

procParens :: (String -> a) -> String -> (a, String)
procParens = runProc unparens

-- Parse up to a sep
fromSep :: Char -> String -> (String, String)
fromSep sep str = let pre  = takeWhile (/= sep) str in
                  let post = drop (length pre + 1) str in
                  (pre, post)

fromSeps :: Char -> String -> [String]
fromSeps _ []  = []
fromSeps sep str = let (a, b)  = fromSep sep str in
                   let as = fromSeps sep b in
                   a:as

runProc :: (String -> (String, String))
        -> (String -> a)
        -> String
        -> (a, String)
runProc t f s = let (a, b) = t s in (f a, b)

--------------------------------------------------------------------------------

size :: Lang -> Int
size (Lang m f) = sumit sizem m + sumit sizef f
  where
  sizem (Mod is es) = length is + length es
  sizef (Func _ as sts) = sumit sizee as + sumit sizes sts
  sizes stmt = case stmt of
    Assign _ e -> 1 + sizee e
    Alloc _ e  -> 1 + sizee e
    Return e   -> 1 + sizee e
  sizee e = case e of
    Int _       -> 1
    Bool _      -> 1
    Add e0 e1   -> 1 + sizee e0 + sizee e1
    Sub e0 e1   -> 1 + sizee e0 + sizee e1
    Mul e0 e1   -> 1 + sizee e0 + sizee e1
    Div e0 e1   -> 1 + sizee e0 + sizee e1
    Not e0      -> 1 + sizee e0
    And e0 e1   -> 1 + sizee e0 + sizee e1
    Or e0 e1    -> 1 + sizee e0 + sizee e1
  sumit sz ls = sum (map sz ls)

--------------------------------------------------------------------------------

prop_parse :: Lang -> Bool
prop_parse e = read' (show' e) == e

scargs :: ScArgs
scargs = scStdArgs { qcArgs  = stdArgs
                                -- { maxSuccess = 1000
                                -- , maxSize    = 20  }
                   , format  = PrintString
                   , runForall  = False
                   , runExists  = False
--                   , scMaxDepth = Just 4
                   , scMaxSize =  5
                   , scMaxReduce = 10
                   }

#if defined(qc) || defined(qcGen) || defined(smart)
main :: IO ()
main = do
  [file', rnds'] <- getArgs
  let rnds = read rnds' :: Int
  let file  = read file' :: String
#ifdef feat
  test file rnds $ runQC' proxy stdArgs {maxSuccess = 1000} prop_parse size
#endif
#if defined(qc) || defined(qcGen)
  test file rnds $ runQC' proxy stdArgs prop_parse size
#endif
#ifdef smart
  test file rnds $ runSC scargs prop_parse size
#endif
#endif
--------------------------------------------------------------------------------

{-
testqc :: IO ()
testqc = quickCheckWith theArgs prop_parse

parseTest :: IO ()
parseTest = smartCheck scargs prop_parse

a0 = Func (Var "foo") [Int 3, Bool True] [Assign (Var "v") (Int 4), Return (Int 5)]
a1 = Assign (Var "a") (Int 0)
a2 = Alloc (Var "a") (Int 0)

runit x = read' $ show' x
-}
