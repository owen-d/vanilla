{-# LANGUAGE DeriveFunctor #-}

module Prob where


import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (forM_, guard)
import qualified Data.Map            as M
import           Text.Printf         (printf)

newtype Prob a = Prob {unProb :: [(a, Float)]} deriving (Functor)
instance Show a => Show (Prob a) where
  show (Prob xs) = concat $
    flip map xs (\(x,p) -> printf "%.3f %s\n" p (show x))

instance Applicative Prob where
    pure x              = Prob [(x, 1)]
    Prob fs <*> Prob xs = Prob
        [(f x, p1 * p2) | (f, p1) <- fs, (x, p2) <- xs]

instance Monad Prob where
    Prob xs >>= f = Prob
        [(y, p1 * p2) | (x, p1) <- xs, (y, p2) <- unProb (f x)]

-- TODO: unsure of how I want to impl (<|>), but using `guard` is too useful
-- Therefore default to (>>)
instance Alternative Prob where
  empty = Prob []
  (<|>) = (>>)

uniform :: [a] -> Prob a
uniform [] = empty
uniform xs = Prob [(x, 1.0/ln) | x <- xs]
  where ln = (fromIntegral . length) xs

run :: (Show a, Ord a) => Prob a -> IO ()
run (Prob xs) =
    forM_ (M.toList $ M.fromListWith (+) xs) $ \(x, p) ->
        printf "%.3f %s\n" p (show x)


-- d6 :: Prob Int
d6 = Prob [(x, 1 / 6) | x <- [1 .. 6]]

-- try:
-- y = run $ do {x <- d6; y <- d6; return (x + y)}
-- What is the chance I do more than 5 damage?
-- z = run $ do {x <- d6; y <- d6; return (x + y > 5)}
x = run $ do
  x <- d6
  y <- d6
  guard (x + y > 5)
  return (x + y > 5)

-- e.g. if any of the dice is <= 2, throw again:
-- o = run $ do {x <- d6; y <- d6; if x <= 2 || y <= 2 then (+) <$> d6 <*> d6 else return (x + y)}
