--module LeastCommonMultiple (lcm) where

import Prelude hiding (lcm)
import Control.Monad
import Data.Maybe

--lcm :: Integral a => [a] -> a
--lcm [] = 1
--lcm [x] = x
--lcm (x:xs) = fromMaybe 0 $ foldM f x xs
--  where f acc c = if c == 0 then Nothing else Just $! (acc * c) `div` gcd acc c


--lcm :: Integral a => [a] -> a
--lcm [] = 1
--lcm [x] = x
--lcm xs = foldr1 f xs
--  where f c acc = let p = c * acc in if p == 0 then 0 else div p (gcd c acc)

--lcm :: Integral a => [a] -> a
--lcm [] = 1
--lcm [x] = x
--lcm (x:xs) = foldl go id xs x where
--  go f acc 0 = 0 
--  go f acc c = f $! c * acc `div` gcd c acc

lcm :: Integral a => [a] -> a
lcm xs = foldr go id xs 1 where
  go 0 f acc = 0
  go c f acc = f $! c * acc `div` gcd c acc

lcm' :: Integral a => [a] -> a -> a
lcm' = foldr go id where
  go 0 f acc = 0
  go c f acc = f $! c

-- The correct way
test :: Integral a => [a] -> a
test [] = 1
test (x:xs) = foldl go id xs x where
  go f acc 0 = 0
  go f acc c = f $! c + acc

mySum :: (Foldable t, Eq n, Num n) => t n -> n
mySum xs = foldr go id xs 0 where
  go 0 f acc = 0
  go x f acc = f $! acc + x

mySum2 :: (Foldable t, Eq n, Num n) => t n -> n
mySum2 = foldr go 0 where
  go 0 acc = 0
  go x acc = acc + x

lcm2 :: Integral a => [a] -> a
lcm2 xs = case xs of
    [] -> 1
    (y:ys) -> if y == 0 then 0 else let z = lcm ys in y * z `quot` gcd y z
