{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
import Data.List (tails, foldl', delete, sort, isPrefixOf, subsequences, nub)
import Data.Maybe (maybeToList, fromMaybe)
import Data.List.Split (splitOn)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tile = White | Black deriving (Eq, Show)

type Floor = Map (Int,Int,Int) Tile

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  let paths = parse <$> input
  putStrLn "Part 1:"
  let flips = (\path -> pathToCoord path (0,0,0)) <$> paths
  let floor = foldl' (\m coord -> Map.alter (\tile -> Just $ flipTile $ fromMaybe White tile) coord m) Map.empty flips
  print $ length $ filter (== Black) $ Map.elems floor

  putStrLn "Part 2:"
  let floor100 = iterate step floor !! 100
  print $ length $ filter (== Black) $ Map.elems floor100

flipTile :: Tile -> Tile
flipTile Black = White
flipTile White = Black

data Dir = E | SE | SW | W | NW | NE deriving (Eq, Show)

parse :: String -> [Dir]
parse [] = []
parse ('s':'e':rest) = SE : parse rest
parse ('s':'w':rest) = SW : parse rest
parse ('n':'e':rest) = NE : parse rest
parse ('n':'w':rest) = NW : parse rest
parse ('e':rest) = E : parse rest
parse ('w':rest) = W : parse rest

dirToCoord :: Dir -> (Int, Int, Int)
dirToCoord E = (1,-1,0)
dirToCoord SE = (0,-1,1)
dirToCoord SW = (-1,0,1)
dirToCoord W = (-1,1,0)
dirToCoord NW = (0,1,-1)
dirToCoord NE = (1,0,-1)

pathToCoord :: [Dir] -> (Int, Int, Int) -> (Int,Int,Int)
pathToCoord [] xyz = xyz
pathToCoord (d:rest) (x,y,z) = pathToCoord rest (x+dx, y+dy, z+dz)
  where
    (dx,dy,dz) = dirToCoord d


step :: Floor -> Floor
step floor =
  let
    coords = Map.keys floor
    x0 = minimum $ (\(x,y,z) -> x) <$> coords
    y0 = minimum $ (\(x,y,z) -> y) <$> coords
    z0 = minimum $ (\(x,y,z) -> z) <$> coords

    x1 = maximum $ (\(x,y,z) -> x) <$> coords
    y1 = maximum $ (\(x,y,z) -> y) <$> coords
    z1 = maximum $ (\(x,y,z) -> z) <$> coords
  in
    Map.fromList $ do
      x <- [x0-1 .. x1+1]
      y <- [y0-1 .. y1+1]
      z <- [z0-1 .. z1+1]
      guard $ x + y + z == 0
      let count = length $ filter (== Black) $ do
            d <-  [E, SE, SW, W, NW, NE]
            pure $ Map.findWithDefault White (pathToCoord [d] (x,y,z)) floor
      pure $ ( (x,y,z)
             , case Map.findWithDefault White (x,y,z) floor of
                 Black | count == 0 || count > 2 -> White
                       | otherwise -> Black
                 White | count == 2 -> Black
                       | otherwise -> White
             )
