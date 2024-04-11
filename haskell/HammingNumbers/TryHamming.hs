
hamming :: Int -> Int
hamming n = (filter isHamming [1,2..]) !! (n - 1)

isHamming :: Int -> Bool
isHamming 1 = True
isHamming x
  | x `mod` 2 == 0 = isHamming $ x `div` 2
  | x `mod` 3 == 0 = isHamming $  x `div` 3
  | x `mod` 5 == 0 = isHamming $ x `div` 5
  | otherwise      = False
