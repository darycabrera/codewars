import Data.Bifunctor
import Data.List

getUnique :: [Int] -> Int
getUnique xs = do
  let (ys, zs) = first sort $ splitAt 3 xs
  let yss = group ys
  foldr f (head ys, False) zs
  where 
    f x acc = if fst acc /= x then (x, True) else acc
