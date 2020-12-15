{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.Char (isDigit, digitToInt)
import Data.List (tails, foldl', delete, sort, isPrefixOf, subsequences)
import Data.List.Split (splitOn)
import Data.Bits ((.|.), (.&.), complement)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)
import Data.Function (fix)
import Numeric (readInt, showIntAtBase)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Grid = Map (Int, Int) Char

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  putStrLn "Part 1:"
  let (_, _, mem) = foldl' step (0, 0, Map.empty) input
  print $ Map.foldl' (+) 0 mem

  putStrLn "Part 2:"
  let (_, mem) = foldl' stepV2 ("", Map.empty) input
  print $ Map.foldl' (+) 0 mem


type State = (Int, Int, Map Int Int)
type StateV2 = (String, Map Int Int)

stepV2 :: StateV2 -> String -> StateV2
stepV2 s stmt =
  if "mask = " `isPrefixOf` stmt
  then setMaskV2 s stmt
  else if "mem" `isPrefixOf` stmt
  then setMemV2 s stmt
  else error $ "bad statment" ++ stmt

setMaskV2 :: StateV2 -> String -> StateV2
setMaskV2 (_, mem) stmt = (mask, mem)
  where
    mask = filter (`elem` "01X") stmt

step :: State -> String -> State
step s stmt =
  if "mask = " `isPrefixOf` stmt
  then setMask s stmt
  else if "mem" `isPrefixOf` stmt
  then setMem s stmt
  else error $ "bad statment" ++ stmt


setMask :: State -> String -> State
setMask (_, _, mem) stmt = (mask0, mask1, mem)
  where
    mask = filter (`elem` "01X") stmt
    mask0 = foldl (\a b -> 2*a + b) 0 $ (charToBit 0 <$> mask)
    mask1 = foldl (\a b -> 2*a + b) 0 $ (charToBit 1 <$> mask)

charToBit :: Int -> Char -> Int
charToBit x 'X' = x
charToBit _ '1' = 1
charToBit _ '0' = 0


setMem :: State -> String -> State
setMem (mask0, mask1, mem) stmt = (mask0, mask1, Map.insert loc value' mem)
  where
    [locS, valueS] = splitOn " = " stmt
    loc :: Int = read $ filter isDigit locS
    value :: Int = read $ valueS
    value' = (value .|. mask0) .&. mask1


setMemV2 :: StateV2 -> String -> StateV2
setMemV2 (mask, mem) stmt = (mask, mem')
  where
    [locS, valueS] = splitOn " = " stmt
    loc :: Int = read $ filter isDigit locS
    value :: Int = read $ valueS
    mem' = foldl' (\m loc -> Map.insert loc value m) mem $ applyMask loc mask

applyMask :: Int -> String -> [Int]
applyMask loc mask = do
  bits :: [Int] <- setBits
  let v :: Int = sum $ fmap (\bit -> 2^(35-bit)) bits
  pure $ loc'' .|. v
  where
    mask1 = (\c -> if c == 'X' then '0' else c) <$> mask
    loc' = loc .|. (readBin mask1)
    xs = fmap fst $ filter (\(i,c) -> c == 'X') $ zip [0..] mask
    setBits = subsequences xs
    loc'' = loc' .&. (complement $ sum $ fmap (\bit -> 2^(35-bit)) xs)


readBin :: Integral a => String -> a
readBin = fst . head . readInt 2 (`elem` "01") digitToInt
