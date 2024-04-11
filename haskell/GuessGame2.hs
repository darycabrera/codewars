module GuessGame (guess) where

import Debug.Trace

guess :: Monad m => (Int -> m Bool) -> m Int
guess gt = do
  iterateFiniteGuesses 7 50 gt
  
iterateFiniteGuesses :: Monad m => Int -> Int -> (Int -> m Bool) -> m Int
iterateFiniteGuesses guessCount currentGuess gt = do
  result <- gt currentGuess
  let nextGuess = deriveNextGuess currentGuess result
  if guessCount == 0 then
    pure nextGuess
  else
    iterateFiniteGuesses (guessCount - 1) nextGuess gt

deriveNextGuess :: Int -> Bool -> Int
deriveNextGuess currentGuess comparisonResult =
  let nextSlice = div currentGuess 2 + mod currentGuess 2
  in
  if comparisonResult then
    currentGuess + nextSlice
  else
    currentGuess - nextSlice

