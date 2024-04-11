module Josephus where

josephus :: [a] -> Int -> [a]
josephus = goAround [] 1

goAround :: [a] -> Int -> [a] -> Int -> [a]
goAround ys index xs k = 
  if null xs then
    reverse ys
  else
    let (remaining, results, currIndex) = oneCycle xs ys k index
    in
    goAround results currIndex (reverse remaining) k

oneCycle :: [a] -> [a] -> Int -> Int -> ([a], [a], Int)
oneCycle remaining results k index = foldl f ([], results, index) remaining
  where f (xs, ys, i) x
          | i == k = (xs, x : ys, 1)
          | otherwise = (x : xs, ys, i + 1)

