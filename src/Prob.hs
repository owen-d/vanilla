{-# LANGUAGE DeriveFunctor #-}

module Prob where


import           Control.Monad (forM_)
import qualified Data.Map      as M
import           Text.Printf   (printf)

newtype Prob a = Prob {unProb :: [(a, Float)]} deriving (Functor, Show)

instance Applicative Prob where
    pure x              = Prob [(x, 1)]
    Prob fs <*> Prob xs = Prob
        [(f x, p1 * p2) | (f, p1) <- fs, (x, p2) <- xs]

instance Monad Prob where
    Prob xs >>= f = Prob
        [(y, p1 * p2) | (x, p1) <- xs, (y, p2) <- unProb (f x)]

run :: (Show a, Ord a) => Prob a -> IO ()
run (Prob xs) =
    forM_ (M.toList $ M.fromListWith (+) xs) $ \(x, p) ->
        printf "%.3f %s\n" p (show x)

-- d6 :: Prob Int
-- d6 = Prob [(x, 1 / 6) | x <- [1 .. 6]]

-- try:
-- y = run $ do {x <- d6; y <- d6; return (x + y)}
-- What is the chance I do more than 5 damage?
-- run $ do {x <- d6; y <- d6; return (x + y > 5)}
-- e.g. if any of the dice is <= 2, throw again:
-- run $ do {x <- d6; y <- d6; if x <= 2 || y <= 2 then (+) <$> d6 <*> d6 else return (x + y)}
