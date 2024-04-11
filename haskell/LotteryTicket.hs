module Bingo where

import Data.Char

bingo :: [(String,Int)] -> Int
bingo = foldr f 0
  where
    f (s, d) acc = if any (\x -> ord x == d) s then acc + 1 else acc
