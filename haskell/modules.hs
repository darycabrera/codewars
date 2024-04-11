import Data.Char

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

encode' :: Int -> String -> String
encode' shift = map (chr . (+ shift) . ord)


decode :: Int -> String -> String
decode shift = encode (negate shift)

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing 
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs

{-
 - Note: It's usually better to use folds for this standard list recursion pattern instead of
 - explicitly writing the recursion because they're easier to read and identify. Everyone knows
 - it's a fold when they see the foldr call, but it takes some more thinking to read explicit
 - recursion.
 -}
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc ) Nothing
