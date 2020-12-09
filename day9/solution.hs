{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List.Split (splitOn)
import Control.Monad (guard)

main :: IO ()
main = do
  input <- filter (not . null) <$> splitOn "\n" <$> readFile("input.txt")
  let numbers :: [Int] = read <$> input
  putStrLn "Part 1:"
  let invalid = head $ search numbers
  putStrLn $ show $ invalid

  putStrLn "Part 2:"
  let set = findSet numbers invalid
  let min' = foldl min (head set) $ tail set
  let max' = foldl max (head set) $ tail set
  putStrLn $ show $ (min' + max')


findSet :: [Int] -> Int -> [Int]
findSet numbers n = head $ do
  start <- [0 .. length numbers - 2]
  size <- [2 .. length numbers - start]
  let set = take size $ drop start numbers
  guard $ foldl (+) 0 set == n
  pure $ set

search :: [Int] -> [Int]
search numbers = do
  i <- [25..length numbers]
  let preamble = take 25 $ drop (i - 25) numbers
  let n = head $ drop i numbers
  guard $ not $ isSumOfPair preamble n
  pure $ n

isSumOfPair :: [Int] -> Int -> Bool
isSumOfPair numbers n = any (== n) $ do
  a <- numbers
  b <- numbers
  guard $ a /= b
  pure $ a + b
