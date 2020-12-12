{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List (tails, foldl', delete, sort)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)
import Data.Function (fix)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Grid = Map (Int, Int) Char

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")

  let grid = Map.fromList $ do
        (r, line) :: (Int, [Char]) <- zip [0..] input
        (c, p) :: (Int, Char) <- zip [0..] line
        pure $ ((r,c), p)

  putStrLn "Part 1:"
  let end = findRepeat step grid
  putStrLn $ show $ Map.foldl' (\a p -> if p == '#' then a + 1 else a) 0 end

  putStrLn "Part 2:"
  let end = findRepeat step' grid
  putStrLn $ show $ Map.foldl' (\a p -> if p == '#' then a + 1 else a) 0 end

findRepeat :: Eq a => (a -> a) -> a -> a
findRepeat f a = let fa = f a in if fa == a then a else findRepeat f fa

-- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
-- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
-- Otherwise, the seat's state does not change.

countAd :: Grid -> (Int, Int) -> Int
countAd grid (r, c) = foldl' (+) 0 $ do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr, dc) /= (0, 0)
  let (r', c') = (r + dr, c + dc)
  pure $ if Map.lookup (r', c') grid == Just '#' then 1 else 0


step :: Grid -> Grid
step grid = Map.mapWithKey rule grid
  where
    rule (r,c) p = case p of
      'L' -> if countAd grid (r, c) == 0 then '#' else p
      '#' -> if countAd grid (r, c) > 3 then 'L' else p
      _ -> p


countSee :: Grid -> (Int, Int) -> Int
countSee grid (r, c) = sum $ do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr, dc) /= (0, 0)
  pure $ (\c -> if c == Just '#' then 1 else 0) $ head $ dropWhile (== Just '.') $ do
    m <- [1..]
    let (r', c') = (r + m * dr, c + m * dc)
    pure $ Map.lookup (r', c') grid

step' :: Grid -> Grid
step' grid = Map.mapWithKey rule grid
  where
    rule (r,c) p = case p of
      'L' -> if countSee grid (r, c) == 0 then '#' else p
      '#' -> if countSee grid (r, c) > 4 then 'L' else p
      _ -> p

