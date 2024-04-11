module CalculatingWithFunctions (plus,minus,times,dividedBy,zero,one,two,three,four,five,six,seven,eight,nine) where

plus :: Int -> Int -> Int
plus = (+)

minus :: Int -> Int -> Int
minus = flip (-)

times :: Int -> Int -> Int
times = (*)

dividedBy :: Int -> Int -> Int
dividedBy = flip div

zero :: (Int -> Int) -> Int
zero = ($ 0)

one :: (Int -> Int) -> Int
one = ($ 1)

two = ($ 2)

three :: Int
three = 3

four :: Int
four = 4

five :: Int
five = 5

six :: Int
six = 6

seven :: Int
seven = 7

eight :: Int
eight = 8

nine :: Int
nine = 9
