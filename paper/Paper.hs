{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Paper where

cast :: SubTypes a => a -> Maybe b
cast _ = undefined

class Arbitrary a where
  -- arbitrary :: a

--type Property a = a -> Bool

data Property

-- sized :: [a] -> Int -> [a]
-- sized = undefined

data SubVal = forall a. (Arbitrary a, SubTypes a) => SubVal a

class Arbitrary a => SubTypes a where
  size    :: a -> Int
  index   :: a -> Int -> Maybe SubVal
  replace :: a -> Int -> SubVal -> a

reduce :: SubTypes a 
       => a -> (a -> Property) -> IO a
reduce cex prop = reduce' 1
  where
  reduce' idx =
    case index cex idx of 
      Nothing -> return cex
      Just v  -> do
        vs <- newVals v
        case test cex idx vs prop of
          Nothing   -> reduce' (idx+1)
          Just cex' -> reduce cex' prop

newVals :: SubVal -> IO [SubVal]
newVals (SubVal a) = 
  sequence (replicate maxTries s)
  where s =     sizedArbitrary a 
            >>= return . SubVal

test :: SubTypes a 
     => a -> Int -> [SubVal] 
     -> (a -> Property) -> Maybe a
test cex idx vs prop = go vs
  where
  go []      = Nothing
  go (v:vs') = 
    let cex' = replace cex idx v in
    if pass prop cex' then go vs' 
      else Just cex'

sizedArbitrary :: forall a. SubTypes a => a -> IO a
sizedArbitrary _ = return (undefined :: a)
--sizedArbitrary a n = SubVal (sized arbitrary n)

maxTries :: Int
maxTries = 100

pass :: (a -> Property) -> a -> Bool
pass _ _ = True

reduce0 :: forall a . SubTypes a 
        => a -> (a -> Property) -> IO a
reduce0 cex prop = reduce' 1
  where
  reduce' idx = 
    case index cex idx of 
      Nothing -> return cex
      Just v  -> 
        case testHole v of
          Just v' -> reduce0 v' prop
          Nothing -> do
            vs <- newVals v
            case test cex idx vs prop of
              Nothing   -> 
                reduce' (idx+1)
              Just cex' -> 
                reduce0 cex' prop

  testHole (SubVal a) = 
    case cast a :: Maybe a of
      Nothing -> Nothing
      Just a' -> 
        if pass prop a' then Nothing
          else Just a'
    
--------------------------------------------------------------------------------

data Tree = L | B Tree Tree

-- instance SubTypes Tree where
--   size L = 1
--   size (B t0 t1) = 1 + size t0 + size 1

--   -- index t 0 = (Just . SubVal) t
--   -- index (B t0 t1) n = if size t0 >= n then 

-- tree = B (B L
--              (B L L))
--           (B L L)

-- size tree = 9

-- index tree 4 = (Just . SubVal) (B L L)

-- index tree 12 = Nothing

-- replace tree 2 (SubVal L) = 
--   B (B L 
--         (B L L))
--      L
