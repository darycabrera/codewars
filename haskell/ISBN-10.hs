module ISBN10 where

import Data.Char
import Data.List

validISBN10 :: String -> Bool
validISBN10 xs = case splitAt 9 xs of
  (ys, [z]) ->
    if any (not . isDigit) ys || not (isDigit z) && z /= 'X' then
      False
    else
      computation (convert xs) == 0
  (_, _) -> False

convert :: String -> [Int]
convert = foldr f []
  where f x acc = if x == 'X' then 10 : acc else digitToInt x : acc

computation :: [Int] -> Int
computation = flip mod 11 . sum . snd . mapAccumL (\acc x -> (acc + 1, x * acc)) 1

