import Data.Char

findMissingLetter :: [Char] -> Char
findMissingLetter [] = '\0'
findMissingLetter (c:cs) = chr . (+ 1) . ord . snd . last . takeWhile (not . fst) $ scanl f (False, c) cs
  where f (_, prev) curr = if ord prev + 2 == ord curr then (True, prev) else (False, curr)

findMissingLetterScan :: [Char] -> [(Bool, Char)]
findMissingLetterScan [] = [(False, ' ')]
findMissingLetterScan (c:cs) = scanl f (False, c) cs
  where f (_, prev) curr = if ord prev + 2 == ord curr then (True, chr (ord prev + 1)) else (False, curr)
