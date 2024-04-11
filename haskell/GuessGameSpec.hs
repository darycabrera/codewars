module GuessGameSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Reader

import GuessGame (guess)

greaterThan :: Int -> Reader Int Bool
greaterThan n = asks (> n)

runGame :: Int -> Int
runGame n = runReader (guess greaterThan) n

spec :: Spec
spec = do
  it "can guess 42" $ property $ do
    runGame 42 `shouldBe` 42
  it "can guess correctly" $ property $ do
    forAll (choose (1, 100)) $ \n ->
      runGame n `shouldBe` n
      
main = hspec spec
