module Josephus where

data Snapshot = Snapshot {
  skip :: Int,
  currentIndex :: String,
  numberLength :: Int,
  result :: Int
}

josephus :: [a] -> Int -> [a]
josephus xs k = undefined

goAround :: [a] -> Int -> Int -> (Int, [a])
goAround [] k i = (0, [])
goAround [x] k i = (0, [x])
goAround xs k i = foldl update xs (i, [])

update :: (Int, [a]) -> a -> (Int, [a])
update (ind, res) y =
  if ind == k then (ind + 1, res ++ y) else 

