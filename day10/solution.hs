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
  putStrLn $ show $ count adapters 0

canConnect :: [Int] -> Int -> [Int]
canConnect available prev = filter (\a -> (a - prev) `elem` [1, 2, 3]) available

count :: [Int] -> Int -> Int
count available prev =
  let
    results :: [Int] = count' <$> [0..]
    goal = maximum available
    count' :: Int -> Int
    count' prev =
      if prev == goal then 1 else
        sum $ (\a -> results !! a) <$> canConnect available prev
  in
    results !! prev
