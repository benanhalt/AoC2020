{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List (tails, scanl', delete, sort)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)
import Data.Function (fix)

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")

  putStrLn "Part 1:"
  let end = findRepeat step input
  putStrLn $ show $ length $ filter (== '#') $ concat end

  putStrLn "Part 2:"
  let end = findRepeat step' input
  putStrLn $ show $ length $ filter (== '#') $ concat end

findRepeat :: Eq a => (a -> a) -> a -> a
findRepeat f a = if f a == a then a else findRepeat f $ f a

-- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
-- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
-- Otherwise, the seat's state does not change.

countAd :: [[Char]] -> (Int, Int) -> Int
countAd s (r, c) = sum $ do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr, dc) /= (0, 0)
  let (r', c') = (r + dr, c + dc)
  guard $ r' >= 0 && r' < length s
  guard $ c' >= 0 && c' < length (s !! r)
  pure $ if (s !! r') !! c' == '#' then 1 else 0

step :: [[Char]] -> [[Char]]
step s = do
  r <- [0..length s - 1]
  pure $ do
    c <- [0..length (s !! r) - 1]
    let p = (s !! r) !! c
    pure $ case p of
      'L' -> if countAd s (r, c) == 0 then '#' else p
      '#' -> if countAd s (r, c) > 3 then 'L' else p
      _ -> p

justCount :: [[Char]] -> [[Char]]
justCount s = do
  r <- [0..length s - 1]
  pure $ do
    c <- [0..length (s !! r) - 1]
    let p = (s !! r) !! c
    pure $ case p of
      'L' -> if countAd s (r, c) == 0 then '#' else p
      '#' -> head $ show $ countAd s (r, c)
      _ -> p

countSee :: [[Char]] -> (Int, Int) -> Int
countSee s (r, c) = sum $ do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr, dc) /= (0, 0)
  pure $ (\c -> if c == '#' then 1 else 0) $ head $ dropWhile (== '.') $ do
    m <- [1..]
    let (r', c') = (r + m * dr, c + m * dc)
    pure $ if r' < 0 || c' < 0 || r' >= length s || c' >= length (s !! r') then 'E' else (s !! r') !! c'

step' :: [[Char]] -> [[Char]]
step' s = do
  r <- [0..length s - 1]
  pure $ do
    c <- [0..length (s !! r) - 1]
    let p = (s !! r) !! c
    pure $ case p of
      'L' -> if countSee s (r, c) == 0 then '#' else p
      '#' -> if countSee s (r, c) > 4 then 'L' else p
      _ -> p
