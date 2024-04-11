module SimpleNumberSequence.Kata (missing) where

import Data.Char
import Data.List

data Snapshot = Snapshot {
    previousNumber :: String,
    currentAccum :: String,
    numberLength :: Int,
    missingAccum :: [Int],
    result :: Int
}

missing :: String -> Maybe Int
missing [] = Nothing
missing [x] = Nothing
missing xs =
  let
    numberIterations = length xs `div` 2
    passSeeds = [1..numberIterations]
    snapshots = fmap (doPassThrough xs) passSeeds
    missingList = concatMap missingAccum snapshots
    missingResults = fmap analyzeResult snapshots
    finalMissingResult = foldl (\acc x -> if x > 0 then x else acc) (-1) missingResults
  in
  if not (null missingList) && finalMissingResult > 0 then
      Just finalMissingResult
  else
      Nothing

analyzeResult :: Snapshot -> Int
analyzeResult snap =
  if not (null (currentAccum snap)) then
    --Sequence is too short - error
    -1
  else
    result snap
    

doPassThrough :: String -> Int -> Snapshot
doPassThrough xs startingNumLen =
  let
    (hd, tl) = splitAt startingNumLen xs
    snapshot = Snapshot {
      previousNumber = hd,
      currentAccum = [],
      numberLength = startingNumLen,
      missingAccum = [],
      result = 0
    }
  in
  foldl updateSnapshot snapshot tl

updateSnapshot :: Snapshot -> Char -> Snapshot
updateSnapshot (Snapshot prev curr len missing result) x =
  let
    previousNumber = read prev :: Int
    expectedNumber = succ previousNumber
    nextNumber = succ expectedNumber
    nextNumLen = length $ show nextNumber

    currAccum = curr ++ [x]
    currAccumLen = length currAccum
    currentNumber = read currAccum :: Int

    -- Number succession from x to x + 1 digits
    newLen = 
      if all (== '9') currAccum then
        len + 1
      else
        len
  in
  if currAccumLen < len then
    -- Consume next digit
    Snapshot { previousNumber = prev, currentAccum = currAccum, numberLength = len, missingAccum = missing, result = result }
  else
    if currentNumber == expectedNumber then
      Snapshot { previousNumber = currAccum, currentAccum = [], numberLength = newLen, missingAccum = missing, result = result}
    else if currentNumber == nextNumber then
      -- Skips one in sequence
      if result == 0 then 
         -- First missing number
        Snapshot { previousNumber = currAccum, currentAccum = [], numberLength = newLen, missingAccum = expectedNumber : missing , result = expectedNumber }
      else
        -- More than one missing number found -- error
        Snapshot { previousNumber = currAccum, currentAccum = [], numberLength = newLen, missingAccum = expectedNumber : missing , result = -1 }
    else if nextNumLen > len then
        -- Number succession from x to x + 1 digits
        Snapshot { previousNumber = prev, currentAccum = currAccum, numberLength = nextNumLen, missingAccum = missing, result = result }
    else
      -- Error in sequence
      Snapshot { previousNumber = prev, currentAccum = [], numberLength = len, missingAccum = missing, result = -1 }

