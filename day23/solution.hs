{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.Char (isDigit, digitToInt)
import Data.List (tails, foldl', break, sort, isPrefixOf, cycle, unfoldr)
import Data.List.Split (splitOn)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data CupGame = CupGame
  { cupOrder :: Map Int Int -- Vector would probably be much faster!
  , currentCup :: Int
  , maxCup :: Int
  } deriving (Show)

main :: IO ()
main = do
  let input = digitToInt <$> "167248359"
  putStrLn "Part 1:"
  let result = solve input 100
  let (pre, post) = break (== 1) $ showCups result
  putStrLn $ concat $ show <$> (drop 1 $ post ++ pre)

  putStrLn "Part 2:"
  let maxInput = maximum input
  let input2 = input ++ [maxInput + 1 .. 1000000]
  let result = solve input2 10000000
  print $ product $ take 2 $ cupsInOrder $ result {currentCup = 1}

solve :: [Int] -> Int -> CupGame
solve input iterations =
  let
    cups = Map.fromList $ zip input (drop 1 $ cycle input)
    game = CupGame cups 1 (maximum input)
  in
    iterate move game !! iterations


cupsInOrder :: CupGame -> [Int]
cupsInOrder CupGame {cupOrder, currentCup, maxCup} = unfoldr (\c -> (\n -> (n,n)) <$> Map.lookup c cupOrder) currentCup

showCups :: CupGame -> [Int]
showCups game@CupGame {currentCup, maxCup} = take maxCup $ cupsInOrder game

countDown :: CupGame -> [Int]
countDown CupGame {currentCup, maxCup} = take maxCup $ (\i -> 1 + i `mod` maxCup) <$> [currentCup - 2, currentCup - 3 ..]

move :: CupGame -> CupGame
move game@CupGame {cupOrder, currentCup, maxCup} =
  let
    removed = take 3 $ cupsInOrder game
    Just removedNext = Map.lookup (last removed) cupOrder
    dest = head $ dropWhile (`elem` removed) $ countDown game
    Just destNext = Map.lookup dest cupOrder
    newOrder = Map.insert dest (head removed) $ Map.insert (last removed) destNext $ Map.insert currentCup removedNext cupOrder
    Just nextCup = Map.lookup currentCup newOrder
  in
    game {cupOrder = newOrder, currentCup = nextCup}


