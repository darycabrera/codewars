guess :: Monad m => (Int -> m Bool) -> m Int
guess gt = do iterateFiniteGuesses 6 50 1 100 gt
  

iterateFiniteGuesses :: Monad m => Int -> Int -> Int -> Int -> (Int -> m Bool) -> m Int
iterateFiniteGuesses guessCount currentGuess lowerBound upperBound gt = do
  result <- gt currentGuess
  let (newLowerBound, newUpperBound, newGuess) = calculateNewGuess lowerBound upperBound currentGuess result
  if guessCount == 0 then
    pure newUpperBound
  else
    iterateFiniteGuesses (guessCount - 1) newGuess newLowerBound newUpperBound gt


calculateNewGuess :: Int -> Int -> Int -> Bool -> (Int, Int, Int)
calculateNewGuess lowerBound upperBound currentGuess comparisonResult =
  if comparisonResult then
      (currentGuess, upperBound, div (currentGuess + upperBound) 2)
  else
      (lowerBound, currentGuess, div (currentGuess + lowerBound) 2)

