{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Dist where


import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (forM_, guard)
import qualified Data.Map            as M
import           Text.Printf         (printf)

class Distable a b | a -> b where
  toDist :: a -> Dist b

newtype Dist a = Dist {unDist :: [(a, Float)]} deriving (Functor)

instance Distable (Dist a) a where
  toDist = id

instance Distable [a] a where
  toDist xs = uniform xs

instance Show a => Show (Dist a) where
  show (Dist xs) = concat $
    flip map xs (\(x,p) -> printf "%.3f %s\n" p (show x))

instance Applicative Dist where
    pure x              = Dist [(x, 1)]
    Dist fs <*> Dist xs = Dist
        [(f x, p1 * p2) | (f, p1) <- fs, (x, p2) <- xs]

instance Monad Dist where
    Dist xs >>= f = Dist
        [(y, p1 * p2) | (x, p1) <- xs, (y, p2) <- unDist (f x)]

instance Foldable Dist where
  foldMap _ (Dist [])         = mempty
  foldMap f (Dist ((x,_):xs)) = f x <> foldMap f (Dist xs)

instance Alternative Dist where
  empty = Dist []
  (Dist (x:xs)) <|> _ = Dist (x:xs)
  _ <|> y = y


uniform :: [a] -> Dist a
uniform [] = empty
uniform xs = Dist [(x, 1.0/ln) | x <- xs]
  where ln = (fromIntegral . length) xs

-- | probability of an event occuring
(??) :: (a -> Bool) -> Dist a -> Float
(??) p = sum . map snd . filter (p . fst) . unDist

-- | distribution that matches a predicate
distWhere :: (a -> Bool) -> Dist a -> Dist a
distWhere f dist = do
  x <- dist
  guard (f x)
  return x

-- | given a base distribution, yields a distribution of the list of choices in each round
rounds :: Int -> Dist a -> Dist [a]
rounds = roundsWith (:) []

-- | version of rounds that uses mappend
roundsM :: (Monoid a) => Int -> Dist a -> Dist a
roundsM = roundsWith (<>) mempty

-- | rounds with a function to map distributions together n times from a starting value
roundsWith :: (a -> b -> b) -> b -> Int -> Dist a -> Dist b
roundsWith _ acc 0 _    = Dist [(acc,1)]
roundsWith f acc n dist = f <$> dist <*> roundsWith f acc (n - 1) dist

-- | adds the probabilties for all occurrences in a distribution
totalProb :: Dist a -> Float
totalProb (Dist xs) = sum $ map snd xs

-- softmax normalizes a Distribution such that all probabilities sum to one
softmax :: Dist a -> Dist a
softmax dist@ (Dist xs) =
  Dist $ flip map xs $ \(x,p) -> (x,p/ps)
  where
    ps = totalProb dist

-- | coalesceWith reduces a distribution to a single value using each occurrence's probability
coalesceWith :: (Float -> a -> b -> b) -> b -> Dist a -> b
coalesceWith _ acc (Dist []) = acc
coalesceWith f acc (Dist ((x,p):xs)) =
  f p x $ coalesceWith f acc (Dist xs)

-- | dedup will combine identical events, summing their probabilities
dedup :: Ord a => Dist a -> Dist a
dedup (Dist xs) =
  Dist $ M.toList $ M.fromListWith (+) xs

-- | run prints the probabilities of a distribution
run :: (Show a, Ord a) => Dist a -> IO ()
run (Dist xs) =
    forM_ (M.toList $ M.fromListWith (+) xs) $ \(x, p) ->
        printf "%.3f %s\n" p (show x)

-- | d6 is a six sided die roll
d6 :: Dist Int
d6 = Dist [(x, 1 / 6) | x <- [1 .. 6]]

-- try:
-- y = run $ do {x <- d6; y <- d6; return (x + y)}
-- What is the chance I do more than 5 damage?
-- z = run $ do {x <- d6; y <- d6; return (x + y > 5)}
x' :: IO ()
x' = run $ do
  x <- d6
  y <- d6
  guard (x + y > 5)
  return (x + y > 5)

-- e.g. if any of the dice is <= 2, throw again:
-- o = run $ do {x <- d6; y <- d6; if x <= 2 || y <= 2 then (+) <$> d6 <*> d6 else return (x + y)}
