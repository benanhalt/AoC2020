{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List.Split (splitOn)
import Control.Monad (guard)

main :: IO ()
main = do
  input <- filter (not . null) <$> splitOn "\n" <$> readFile("input.txt")
  let numbers :: [Int] = read <$> input

  putStrLn "Part 1:"
  let invalid = findInvalid numbers
  putStrLn $ show $ invalid

  putStrLn "Part 2:"
  let set = findSet numbers invalid
  let min' = minimum set
  let max' = maximum set
  putStrLn $ show $ (min' + max')


findSet :: [Int] -> Int -> [Int]
findSet numbers n = head $ do
  start <- [0 .. length numbers - 2]
  size <- [2 .. length numbers - start]
  let set = take size $ drop start numbers
  guard $ sum set == n
  pure $ set

findInvalid :: [Int] -> Int
findInvalid numbers = head $  do
  i <- [0..length numbers]
  let preamble = take 25 $ drop i numbers
  let n = head $ drop (i + 25) numbers
  guard $ not $ isSumOfPair preamble n
  pure $ n

isSumOfPair :: [Int] -> Int -> Bool
isSumOfPair numbers n = any (== n) $ do
  a <- numbers
  b <- numbers
  guard $ a /= b
  pure $ a + b
