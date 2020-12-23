{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.Char (isDigit, digitToInt)
import Data.List (tails, foldl', delete, sort, isPrefixOf, subsequences)
import Data.Maybe (maybeToList)
import Data.List.Split (splitOn)
import Data.Bits ((.|.), (.&.), complement)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)
import Data.Function (fix)
import Numeric (readInt, showIntAtBase)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Grid = Map (Int, Int, Int, Int) Char
type Bounds = ((Int, Int, Int, Int), (Int, Int, Int, Int))

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  let grid = Map.fromList $ do
        (r, line) <- zip [0..] input
        (c, p) <- zip [0..] line
        return ((r, c, 0, 0), p)

  let bounds = ((0,0,0,0), last $ Map.keys grid)
  putStrLn "Part 1:"
  print $ Map.foldl' (\count p -> if p == '#' then count + 1 else count) 0 $ snd $ iterate step (bounds, grid) !! 6

-- If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
-- If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.

step :: (Bounds, Grid) -> (Bounds, Grid)
step (((x,y,z,w), (xx,yy,zz,ww)), grid) = (((x-1,y-1,z-1,w-1), (xx+1,yy+1,zz+1,ww+1)), grid')
  where
    grid' = Map.fromList $ do
      r <- [x-1..xx+1]
      c <- [y-1..yy+1]
      z <- [z-1..zz+1]
      w <- [w-1..ww+1]
      pure $ ((r,c,z,w), rule (r,c,z,w) $ Map.findWithDefault '.' (r,c,z,w) grid)

    rule loc '#' = case count loc of
      2 -> '#'
      3 -> '#'
      _ -> '.'
    rule loc '.' = case count loc of
      3 -> '#'
      _ -> '.'

    count (r, c, z, w) = length $ filter (== '#') $ do
      dr <- [-1..1]
      dc <- [-1..1]
      dz <- [-1..1]
      dw <- [-1..1]
      guard $ (dr, dc, dz, dw) /= (0,0,0,0)
      maybeToList $ Map.lookup (r+dr, c+dc, z+dz, w+dw) grid
