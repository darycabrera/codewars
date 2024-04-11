cleanString :: String -> String
cleanString = reverse . foldl processChar []

processChar :: String -> Char -> String
processChar [] x = [x | x /= '#']
processChar (y:ys) x = if x /= '#' then x:y:ys else ys

