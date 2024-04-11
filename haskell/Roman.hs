module Roman where

import Data.Map as Map
import Data.Maybe as Maybe

solution :: String -> Int
solution =
  let
    m =
      Map.fromList [('M', 1000), ('D', 500), ('C', 100), ('L', 50), ('X', 10), ('V', 5), ('I', 1)]
  in decode . Maybe.mapMaybe (`Map.lookup` m)

decode :: [Int] -> Int
decode [] = 0
decode [x] = x
decode (x:y:ys) = if x < y then decode (y - x : ys) else x + decode (y:ys)

