module Josephus where

josephus :: [a] -> Int -> [a]
josephus [] k = []
josephus [x] k = [x]
josephus xs k = 
  let
    n = length xs
    ks = take n $ iterate (+k) (k - 1)
  in
  goAround ks (cycle xs)

goAround :: [Int] -> [a] -> [a]
goAround ks xs = reverse $ foldl (\acc k -> xs !! k : acc) [] ks

