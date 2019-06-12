{-# LANGUAGE DeriveFunctor #-}

module Dist where


import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (forM_, guard)
import qualified Data.Map            as M
import           Text.Printf         (printf)

newtype Dist a = Dist {unDist :: [(a, Float)]} deriving (Functor)
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

-- TODO: unsure of how I want to impl (<|>), but using `guard` is too useful
-- Therefore default to (>>)
instance Alternative Dist where
  empty = Dist []
  (<|>) = (>>)

uniform :: [a] -> Dist a
uniform [] = empty
uniform xs = Dist [(x, 1.0/ln) | x <- xs]
  where ln = (fromIntegral . length) xs

-- probability of an event occuring
(??) :: (a -> Bool) -> Dist a -> Float
(??) p = sum . map snd . filter (p . fst) . unDist

run :: (Show a, Ord a) => Dist a -> IO ()
run (Dist xs) =
    forM_ (M.toList $ M.fromListWith (+) xs) $ \(x, p) ->
        printf "%.3f %s\n" p (show x)


-- d6 :: Dist Int
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
