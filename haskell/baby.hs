doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <-st, c `elem` ['A'..'Z']]

--addThree :: Int -> Int -> Int -> Int
--addThree x y z = x + y + z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Whoops, empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2 
          skinny = 18.5
          normal = 25.0
          fat = 30.0

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     =  GT
    | a == b    = EQ
    | otherwise = LT


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' [] [] = "Whoops, no valid strings"
initials' _ [] = "No last name"
initials' [] _ = "No first name"
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

--let <bindings> in <expression>
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * h * r
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- pattern matching implementation of head function
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

-- case expression implementation of head function
head2 :: [a] -> a
head2 xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "singleton list."
                                               xs  -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ whatIsIt xs
    where whatIsIt [] = "empty."
          whatIsIt [x] = "singleton list."
          whatIsIt xs = "a longer list."

--Recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty lists."
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "maximum of empty lists."
maximum2 [x] = x
maximum2 (x:xs) = max x (maximum2 xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0   = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

--Using list comprehensions
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--Using the filter function
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

--Partially applied infix functions
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

--higher order functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--Naive implementation of flip
flip1 :: (a -> b -> c) -> (b -> a -> c)
flip1 f = g
    where g x y = f y x

--Simpler implementation of flip
flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f y x = f x y

--Maps
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs)  = f x : map' f xs

--Filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

--Collatz Conjecture
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
    | even x = x:chain (x `div` 2)
    | odd x = x:chain (x*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

--Lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain[1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

--Illustrating function currying
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z ->  x + y + z

--flip with lambda
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

--Folds
sumf :: (Num a) => [a] -> a
sumf xs = foldl(\acc x -> acc + x) 0 xs

--Curried fold
sumf' :: (Num a) => [a] -> a
sumf' = foldl (+) 0

--Left folds continued
elemf :: (Eq a) => a -> [a] -> Bool
elemf y ys = foldl (\acc x -> if x == y then True else acc) False ys

--Right fold implement map
mapf :: (a -> b) -> [a] -> [b]
mapf f xs = foldr (\x acc -> f x : acc) [] xs

--Left fold implement map
--This is inefficient because the (++) function is more expensive than pre-pending with :
mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

--Implement reverse with left fold exercise
reversef :: [a] -> [a]
reversef xs = foldl (\acc x -> x : acc) [] xs

--Implement reverse with left fold but leverage currying
--Generally if you have a function like foo a = bar b a, you can rewrite it as foo = bar b, because of currying
reversefa :: [a] -> [a]
reversefa = foldl (\acc x -> x : acc) []

--Implement reverse with left fold with (:)
reversefb :: [a] -> [a]
reversefb = foldl (flip (:)) []

--how many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
