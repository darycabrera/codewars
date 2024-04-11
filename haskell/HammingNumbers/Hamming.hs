--hamming  :: Int -> Int
--hamming n = h !! (n-1)
--    where h = 1 : map (*2) h `f` map (*3) h `f` map (*5) h
--            where f xxs@(x:xs) yys@(y:ys)
--                    | x==y = x : f xs ys
--                    | x<y  = x : f xs yys
--                    | x>y  = y : f xxs ys

hAux = 1:foldl u [] [5,3,2] where
  u s n = ar where
    ar = merge s (n:map (n*) ar)
    merge [] b = b
    merge a@(x:xs) b@(y:ys)
      | x < y     = x:merge xs b
      | otherwise = y:merge a ys

hamming  :: Int -> Int
hamming n = hAux !! (n - 1)

--hamming  :: Int -> Int
--hamming n = hammingNumbers !! (n - 1)
--
--hammingNumbers :: [Int]
--hammingNumbers = 1 : mergeAll [map (2*) hammingNumbers,
--                               map (3*) hammingNumbers,
--                               map (5*) hammingNumbers]
--
--merge :: Ord a => [a] -> [a] -> [a]
--merge []     ys = ys
--merge xs     [] = xs
--merge (x:xs) (y:ys) = case compare x y of
--                        EQ -> x : merge xs ys
--                        LT -> x : merge xs (y:ys)
--                        GT -> y : merge (x:xs) ys
--
--mergeAll :: Ord a  => [[a]] -> [a]
--mergeAll = foldr merge []
