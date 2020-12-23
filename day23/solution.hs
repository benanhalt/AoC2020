{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.Char (isDigit, digitToInt)
import Data.List (tails, foldl', break, sort, isPrefixOf, subsequences)
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
  let input = "167248359"
  putStrLn "Part 1:"
  let result = iterate move (digitToInt <$> input) !! 100
  let (pre, post) = break (== 1) result
  putStrLn $ concat $ show <$> post ++ pre

  putStrLn "Part 2:"
  let cups = digitToInt <$> input
  let maxCup = maximum cups
  let allCups = take 1000000 $ cups ++ [maxCup + 1 ..]
  let result2 = iterate move allCups !! 100
  let (pre2, post2) = break (== 1) result2
  print $ take 2 $ drop 1 post2

  
move :: [Int] -> [Int]
move cups =
  let
    current = head cups
    removed = drop 1 $ take 4 cups
    remaining = current : drop 4 cups
    lessThanCur = filter (< current) $ sort $ remaining
    dest = if null lessThanCur then maximum remaining else last lessThanCur
    (pre, post) = break (== dest) remaining
    placed = pre ++ [head post] ++ removed ++ tail post
  in
    (drop 1 placed) ++ [head placed]

-- The crab picks up the three cups that are immediately clockwise of
-- the current cup. They are removed from the circle; cup spacing is
-- adjusted as necessary to maintain the circle.  The crab selects a
-- destination cup: the cup with a label equal to the current cup's
-- label minus one. If this would select one of the cups that was just
-- picked up, the crab will keep subtracting one until it finds a cup
-- that wasn't just picked up. If at any point in this process the
-- value goes below the lowest value on any cup's label, it wraps
-- around to the highest value on any cup's label instead.  The crab
-- places the cups it just picked up so that they are immediately
-- clockwise of the destination cup. They keep the same order as when
-- they were picked up.  The crab selects a new current cup: the cup
-- which is immediately clockwise of the current cup
