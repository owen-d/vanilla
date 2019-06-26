module Engine.Dist.Joinable where

import           Engine.Dist

-- join is syntactic sugar for joining all distributions with the same weight (1)
join :: Distable a b => [a] ->  Dist b
join xs = joinWeights $ map (\x -> (x,1)) xs

-- joinWeights weighs a list of distributions according to the provided weights and concats the results
joinWeights :: Distable a b => [(a,Float)] -> Dist b
joinWeights [] = Dist []
joinWeights ((x,w):xs) =
  weigh w x `append` (joinWeights xs)

-- append concats all events in multiple distributions but does not adjust their weights
append :: Distable a b => a -> a -> Dist b
append x y = Dist $ (unDist . toDist) x ++ (unDist . toDist) y

-- weigh will adjust the probability of distribution by a certain weight
-- `weigh 0.3 x` multiplies all cases in the distribution x by 0.3
weigh :: Distable a b => Float -> a -> Dist b
weigh w d = do
  d' <- toDist d
  _ <- Dist [((), w)]
  return d'
