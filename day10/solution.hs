{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List (tails, scanl', delete, sort)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)

main :: IO ()
main = do
  input <- words <$> readFile("input.txt")
  let adapters :: [Int] = read <$> input

  putStrLn "Part 1:"
  let sorted = sort $ (0: adapters)
  let diffs = 3 : zipWith (-) (tail sorted) sorted
  let ones = length $ filter (== 1) diffs
  let threes = length $ filter (== 3) diffs
  putStrLn $ show $ ones * threes

  putStrLn "Part 2:"
  putStrLn $ show sorted
  putStrLn $ show diffs
  putStrLn $ show $ chain adapters 0 0

canConnect :: [Int] -> Int -> [Int]
canConnect available prev = filter (\a -> (a - prev) `elem` [1, 2, 3]) available

chain :: [Int] -> Int -> Int -> Int
chain available generated current =
  if null $ canConnect available current
  then generated + 1
  else sum $ do
    next <- canConnect available current
    pure $ chain (delete next available) generated next
