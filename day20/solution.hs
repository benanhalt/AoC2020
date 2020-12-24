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

solution :: Tiling
solution = Map.fromList [((0,0),(3557,False,0)),((0,1),(1061,True,2)),((0,2),(1913,True,0)),((0,3),(3907,False,2)),((0,4),(1427,True,0)),((0,5),(1559,True,3)),((0,6),(1511,False,0)),((0,7),(2887,False,0)),((0,8),(2029,True,0)),((0,9),(2593,True,3)),((0,10),(2423,True,3)),((0,11),(1019,False,0)),((1,0),(3739,False,0)),((1,1),(2551,False,1)),((1,2),(3061,False,1)),((1,3),(1367,False,0)),((1,4),(3301,True,3)),((1,5),(2657,False,3)),((1,6),(3527,False,1)),((1,7),(2957,False,3)),((1,8),(1307,False,3)),((1,9),(1291,False,2)),((1,10),(2851,True,2)),((1,11),(2777,False,2)),((2,0),(2347,True,3)),((2,1),(2843,False,2)),((2,2),(2699,True,2)),((2,3),(3049,False,2)),((2,4),(2003,False,2)),((2,5),(2069,True,0)),((2,6),(1753,False,2)),((2,7),(3541,False,1)),((2,8),(1543,False,3)),((2,9),(2333,True,0)),((2,10),(2473,True,3)),((2,11),(1171,False,2)),((3,0),(3469,False,3)),((3,1),(3119,False,1)),((3,2),(1181,True,0)),((3,3),(3761,False,3)),((3,4),(3163,True,2)),((3,5),(1487,True,0)),((3,6),(1483,True,0)),((3,7),(1283,False,0)),((3,8),(1973,True,0)),((3,9),(1657,False,1)),((3,10),(3041,False,3)),((3,11),(2297,True,3)),((4,0),(2179,False,3)),((4,1),(3121,True,0)),((4,2),(3517,True,3)),((4,3),(1949,True,3)),((4,4),(3169,False,1)),((4,5),(3083,True,2)),((4,6),(1847,False,2)),((4,7),(1789,False,2)),((4,8),(3109,True,2)),((4,9),(2879,False,0)),((4,10),(3821,False,0)),((4,11),(1163,False,0)),((5,0),(1933,True,2)),((5,1),(1039,False,2)),((5,2),(2549,False,2)),((5,3),(1597,True,3)),((5,4),(2741,False,1)),((5,5),(2713,True,0)),((5,6),(3257,True,0)),((5,7),(3659,True,0)),((5,8),(1901,False,2)),((5,9),(1723,True,0)),((5,10),(1579,False,1)),((5,11),(3181,True,3)),((6,0),(3677,False,2)),((6,1),(1213,False,2)),((6,2),(1889,False,3)),((6,3),(3767,True,2)),((6,4),(1621,False,3)),((6,5),(1693,True,3)),((6,6),(3643,False,0)),((6,7),(1289,False,3)),((6,8),(3637,False,1)),((6,9),(2143,False,2)),((6,10),(3037,True,2)),((6,11),(2539,True,3)),((7,0),(1741,True,0)),((7,1),(1499,False,3)),((7,2),(1009,False,0)),((7,3),(2617,False,0)),((7,4),(2801,False,0)),((7,5),(2693,True,2)),((7,6),(3671,True,3)),((7,7),(2083,False,3)),((7,8),(3989,True,2)),((7,9),(3797,False,1)),((7,10),(1049,True,0)),((7,11),(2647,True,3)),((8,0),(3613,False,1)),((8,1),(1783,True,0)),((8,2),(3673,False,0)),((8,3),(3407,False,2)),((8,4),(2441,True,2)),((8,5),(2087,False,0)),((8,6),(3697,True,2)),((8,7),(2557,False,1)),((8,8),(1867,False,3)),((8,9),(1381,False,1)),((8,10),(1997,False,3)),((8,11),(1663,False,0)),((9,0),(2389,True,2)),((9,1),(2017,False,3)),((9,2),(3331,False,1)),((9,3),(2803,True,3)),((9,4),(2927,True,0)),((9,5),(1493,False,2)),((9,6),(3079,True,0)),((9,7),(2477,True,0)),((9,8),(1811,False,0)),((9,9),(1567,False,3)),((9,10),(3299,True,3)),((9,11),(1553,True,3)),((10,0),(3691,False,1)),((10,1),(1861,False,1)),((10,2),(1987,False,0)),((10,3),(2683,False,3)),((10,4),(2399,False,1)),((10,5),(1583,False,2)),((10,6),(1733,False,3)),((10,7),(2663,True,3)),((10,8),(1447,False,0)),((10,9),(1223,True,2)),((10,10),(2749,False,0)),((10,11),(3089,True,2)),((11,0),(1097,True,2)),((11,1),(3923,False,2)),((11,2),(2999,True,2)),((11,3),(2797,True,2)),((11,4),(1409,False,1)),((11,5),(3359,False,1)),((11,6),(1327,True,0)),((11,7),(2309,False,3)),((11,8),(3361,False,1)),((11,9),(2917,False,3)),((11,10),(1823,True,0)),((11,11),(3769,True,2))]
