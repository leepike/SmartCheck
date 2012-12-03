{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Paper where

class Arbitrary a where
  -- arbitrary :: a

type Property a = a -> Bool

-- sized :: [a] -> Int -> [a]
-- sized = undefined

data SubVal = forall a. (Arbitrary a, SubTypes a) => SubVal a

class Arbitrary a => SubTypes a where
  size    :: a -> Int
  index   :: a -> Int -> Maybe SubVal
  replace :: a -> Int -> SubVal -> a

reduce :: SubTypes a => a -> Property a -> IO a
reduce cex prop = reduce' 1
  where
  reduce' idx = 
    case index cex idx of 
      Nothing -> return cex
      Just v  -> 
        case test cex idx v prop of
          Nothing   -> reduce' (idx + 1)
          Just cex' -> reduce cex' prop

test :: SubTypes a 
     => a -> Int -> SubVal -> Property a -> Maybe a
test cex idx v prop = go maxTries
  where
  go 0 = Nothing
  go n = let v'   = newVal v in
         let cex' = replace cex idx v' in
         if prop cex' then go (n - 1) else Just cex'

  newVal (SubVal a) = sizedArbitrary a (size a)

sizedArbitrary :: forall a. SubTypes a => a -> Int -> SubVal
sizedArbitrary _ _ = SubVal (undefined :: a)
--sizedArbitrary a n = SubVal (sized arbitrary n)

maxTries :: Int
maxTries = 100    
