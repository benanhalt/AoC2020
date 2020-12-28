{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
import qualified Data.Text as T
import Data.Char (isDigit, digitToInt)
import Data.List (tails, foldl', delete, sort, isPrefixOf, subsequences, cycle, transpose)
import Data.Maybe (maybeToList)
import Data.List.Split (splitOn)
import Data.Bits ((.|.), (.&.), complement)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)
import Data.Function (fix)
import Numeric (readInt, showIntAtBase)
import Data.Attoparsec.Text (Parser, parseOnly, anyChar, choice, char, digit, peekChar)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- data Tile = Tile
--   { tileId :: Int
--   , tilePattern :: [String]
--   } deriving (Eq, Ord, Show)

type Tile = Int

parseTile :: [String] -> (Tile, [String])
parseTile (title:pat) = (read $ filter isDigit title, pat)

showTile :: (Tile, [String]) -> String
showTile (tileId, tilePattern)  = unlines $ (show tileId : tilePattern)

type Boundary = [String]

boundary :: (Tile, [String]) -> Boundary
boundary (_, pat) = [head pat, last <$> pat, reverse $ last pat, head <$> (reverse pat)]

flipB :: Boundary -> Boundary
flipB b = reverse (reverse <$> b)

rotB :: Int -> Boundary -> Boundary
rotB r b = take 4 $ drop r $ cycle b

data Dir = U | R | D | L deriving (Eq, Ord, Show)

match :: Boundary -> Dir -> Boundary -> Bool
match t U s = (head t) == (reverse $ s !! 2)
match t R s = match (rotB 1 t) U (rotB 1 s)
match t D s = match (rotB 2 t) U (rotB 2 s)
match t L s = match (rotB 3 t) U (rotB 3 s)

flipP :: [String] -> [String]
flipP = transpose

rotP :: Int -> [String] -> [String]
rotP 0 pat = pat
rotP 1 pat = transpose $ reverse <$> pat
rotP n pat = rotP (n-1) $ rotP 1 pat

stripB :: [String] -> [String]
stripB pat = (init.tail) <$> (init $ tail pat)

type PlacedTile = (Tile, Bool, Int)

type Joinable = [(PlacedTile, Dir, PlacedTile)]

type Tiling = Map (Int,Int) PlacedTile


monsterS =
  ["                  # "
  ,"#    ##    ##    ###"
  ," #  #  #  #  #  #   "
  ]

monster :: Map (Int,Int) Char
monster = Map.fromList $ do
  (r, s) <- zip [0..] monsterS
  (c, p) <- zip [0..] s
  pure ((r,c), p)

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  let tiles = parseTile <$> splitOn [""] input

  putStrLn "Part 1:"
  -- putStrLn $ unlines $ showTile <$> take 4 tiles
  -- print $ boundary $ tiles !! 3
  -- print $ flipB $ boundary $ tiles !! 3
  -- print $ rotB 1 $ boundary $ tiles !! 3

  let joinable :: Joinable = do
        t <- tiles
        s <- tiles
        guard $ t /= s
        let bt = boundary t
        let bs = boundary s
        flipT <- [False, True]
        flipS <- [False, True]
        rotT <- [0..3]
        rotS <- [0..3]
        let bt' = rotB rotT $ if flipT then flipB bt else bt
        let bs' = rotB rotS $ if flipS then flipB bs else bs
        dir <- [U, R, D, L]
        guard $ match bt' dir bs'
        pure ((fst t, flipT, rotT), dir, (fst s, flipS, rotS))

  let solution = head $ solve joinable (fst <$> tiles) Map.empty
  print $ product $ do
    c <- [(0,0), (0,11), (11,0), (11,11)]
    let Just (tileId, _, _) =  Map.lookup c solution
    pure tileId

  putStrLn "Part 2:"
  let fullMap = assemble $ Map.map (xForm tiles) solution
  -- putStrLn $ unlines $ assemble $ fullMap
  let monsters = sum $ do
        i <- [fullMap, flipP fullMap]
        n <- [0..3]
        pure $ length $ findMonsters $ rotP n i
  let allHashes = length $ filter (== '#') $ concat fullMap
  let monsterHashes = length $ filter (== '#') $ Map.elems monster
  print $ allHashes - monsters * monsterHashes

findMonsters :: [String] -> [(Int,Int)]
findMonsters image = do
  r <- [0 .. length image - 1 - 3]
  let row = image !! r
  c <- [0 .. length row - 1 - 20]
  guard $ and $ do
    r' <- [0 .. 2]
    c' <- [0 .. 19]
    monsterP <- maybeToList $ Map.lookup (r',c') monster
    let imageP = (image !! (r+r')) !! (c+c')
    pure $ monsterP /= '#' || imageP == '#'
  pure (r,c)

assemble :: Map (Int,Int) [String] -> [String]
assemble s = do
  r <- [0 .. 12*8 - 1]
  pure $ do
    c <- [0 .. 12*8 - 1]
    let Just tile = Map.lookup (div r 8, div c 8) s
    pure $ (tile !! (mod r 8)) !! mod c 8

xForm :: [(Int, [String])] -> (Int, Bool, Int) -> [String]
xForm tiles (tileId, doFlip, nRot) =
  stripB $ rotP nRot $ if doFlip then flipP pat else pat
  where
    Just pat = tileId `lookup` tiles

solve :: Joinable -> [Tile] -> Tiling -> [Tiling]
solve _ [] tiling = [tiling]
solve joinable tiles tiling =
  let
    (r,c) = nextTile tiling
    neighbors = neighboring tiling (r,c)
  in
    do
      t <- tiles
      flipT <- [False, True]
      rotT <- [0..3]
      let pt = (t,flipT,rotT)
      guard $ all (\(d,n) -> (pt,d,n) `elem` joinable) neighbors
      solve joinable (delete t tiles) $ Map.insert (r,c) pt tiling


neighboring :: Tiling -> (Int,Int) -> [(Dir, PlacedTile)]
neighboring tiling (r,c) = do
  d <- [U,R,D,L]
  let (dr, dc) = dirToCoord d
  t <- maybeToList $ Map.lookup (r+dr, c+dc) tiling
  pure (d, t)

dirToCoord :: Dir -> (Int,Int)
dirToCoord U = (-1, 0)
dirToCoord R = (0, 1)
dirToCoord D = (1, 0)
dirToCoord L = (0, -1)


nextTile :: Tiling -> (Int, Int)
nextTile tiling = case Map.lookupMax tiling of
  Nothing -> (0,0)
  Just ((r,c), _) | c+1 == 12 -> (r+1,0)
                  | otherwise -> (r, c+1)
