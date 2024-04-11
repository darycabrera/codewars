import qualified Data.List as List

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

levelOrdered :: Tree a -> [a]
levelOrdered = fmap snd . List.sortOn fst . getLevels 0

getLevels :: Int -> Tree a -> [(Int, a)]
getLevels level EmptyTree = []
getLevels level (Node a left right) = (level, a) : getLevels (level + 1) left ++ getLevels (level + 1) right 

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeSort :: Tree a -> [a]
treeSort EmptyTree = []
treeSort (Node a left right) = [a] ++ treeSort left ++ treeSort right

