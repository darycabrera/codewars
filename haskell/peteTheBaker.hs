import Data.List

cakes :: [(String, Int)] -> [(String, Int)] -> Int
cakes recipe storage  =
  minimum $ fmap (\x -> maybe 0 (`div` snd x) . flip lookup storage $ fst x) recipe
