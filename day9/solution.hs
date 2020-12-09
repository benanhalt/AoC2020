{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List (tails, scanl')
import Control.Monad (guard)

main :: IO ()
main = do
  input <- words <$> readFile("input.txt")
  let numbers :: [Int] = read <$> input

  putStrLn "Part 1:"
  let invalid = findInvalid numbers
  putStrLn $ show $ invalid

  putStrLn "Part 2:"
  putStrLn $ show $ findWeakness numbers invalid


findWeakness :: [Int] -> Int -> Int
findWeakness numbers n = head $ do
  t <- tails numbers
  first <- take 1 t
  let rest = tail t
  let sums = scanl' (\(sum', min', max') x -> (sum' + x, min min' x, max max' x)) (first, first, first) $ rest
  let nonDegenerate = drop 1 sums -- Throw away the degenerate case where the sum is a single term.
  let sumsNotLessThanN = dropWhile (\(sum', _, _) -> sum' < n) nonDegenerate
  (sum', min', max') <- take 1 sumsNotLessThanN -- Only need to check the first sum >= n since they are monotonically increasing.
  guard $ sum' == n
  pure $ min' + max'

findInvalid :: [Int] -> Int
findInvalid numbers = head $ do
  t <- tails numbers
  let (preamble, rest) = splitAt 25 t
  n <- take 1 rest
  guard $ not $ any (== n) $ allSums preamble
  pure $ n

allSums :: [Int] -> [Int]
allSums numbers = do
  t <- tails numbers
  a <- take 1 t
  b <- tail t
  pure $ a + b
