import qualified Data.List as List
import qualified Data.Set as Set

generateNthHammingNumber :: Int -> Int
generateNthHammingNumber n = last . take (max 1 n) $ generateHammingNumbers [1] 1

generateHamster :: [Int]
generateHamster = take 5000 $ generateHammingNumbers [1] 1

generateHammingNumbers :: [Int] -> Int -> [Int]
generateHammingNumbers xs level =
 let gen = List.sort . List.nub . filter ( < 2 ^ level ) . (++) xs $ [(* 2), (* 3), (* 5)] <*> xs
 in List.union gen $ generateHammingNumbers gen (level + 1)
  
generateHamster1 :: [Int]
generateHamster1 = List.sort . Set.toList $ generate1 1 Set.empty
  
generate1 :: Int -> Set.Set Int -> Set.Set Int
generate1 n s =
    let
      ns = Set.insert n s
      x = if div n 2 < 30 then generate1 (n * 2) ns else ns
      y = if div n 3 < 30 then generate1 (n * 3) ns else ns
      z = if div n 5 < 30 then generate1 (n * 5) ns else ns
    in
    Set.unions [x, y, z]
