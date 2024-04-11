module ConvertCase where

import Data.Char
import Data.List
import Data.List.Split

changeCase :: String -> String -> Maybe String
changeCase xs ys =
  case ys of
    "snake" ->
      if isSnakeCase xs then
        Just xs
      else if isCamelCase xs then
        Just $ changeToSnakeCase xs "camel"
      else if isKebabCase xs then
        Just $ changeToSnakeCase xs "kebab"
      else
        Nothing
    "camel" ->
      if isCamelCase xs then
        Just xs
      else if isSnakeCase xs then
        Just $ changeToCamelCase xs "snake"
      else if isKebabCase xs then
        Just $ changeToCamelCase xs "kebab"
      else
        Nothing
    "kebab" ->
      if isKebabCase xs then
        Just xs
      else if isCamelCase xs then
        Just $ changeToKebabCase xs "camel"
      else if isSnakeCase xs then
        Just $ changeToKebabCase xs "snake"
      else
        Nothing
    _ ->
      Nothing

isCamelCase :: String -> Bool
isCamelCase = all (\c -> c /= '_' && c /= '-')

isSnakeCase :: String -> Bool
isSnakeCase = all (\c -> c == '_' || isLower c)

isKebabCase :: String -> Bool
isKebabCase = all (\c -> c == '-' || isLower c)

changeToCamelCase :: String -> String -> String
changeToCamelCase [] _ = []
changeToCamelCase xs ys =
  case ys of
    "kebab" -> camelCaseWords $ splitOn "-" xs
    "snake" -> camelCaseWords $ splitOn "_" xs
    _ -> []

changeToKebabCase :: String -> String -> String
changeToKebabCase [] _ = []
changeToKebabCase xs ys =
   case ys of
     "camel" -> convertFromCamelCaseWith '-' xs
     "snake" -> findAndReplace '_' '-' xs
     _ -> []

changeToSnakeCase :: String -> String -> String
changeToSnakeCase _ [] = []
changeToSnakeCase xs ys =
  case ys of
    "camel" -> convertFromCamelCaseWith '_' xs
    "kebab" -> findAndReplace '-' '_' xs
    _ -> []

camelCaseWords :: [String] -> String
camelCaseWords [] = []
camelCaseWords (x:xs) = concat $ x : fmap capitalizeWord xs

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

findAndReplace :: Char -> Char -> String -> String
findAndReplace k r =
  foldr (\c acc -> if c == k then r:acc else c:acc) []

convertFromCamelCaseWith :: Char -> String -> String
convertFromCamelCaseWith k =
  foldr (\c acc -> if isUpper c then k : toLower c : acc else c:acc) []

split :: Char -> String -> [String]
split delimiter = foldr f [[]]
  where f c ~(a:acc) = if c == delimiter then []:a:acc else (c:a):acc

