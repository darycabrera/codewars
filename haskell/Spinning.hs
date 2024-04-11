module Codewars.Kata.Spinning where

spinWords :: String -> String
spinWords = unwords . fmap f . words where
  f = \xs -> if length xs >= 5 then reverse xs else xs

