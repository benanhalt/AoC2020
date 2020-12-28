{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
import Data.List (tails, foldl', delete, sort, isPrefixOf, subsequences)
import Data.List.Split (splitOn)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Rule
  = Literal Char
  | Alts [Rule]
  | Refs [String]
  deriving (Eq, Show)

parseRule :: String -> Rule
parseRule s | " \"" `isPrefixOf` s = Literal $ s !! 2
            | '|' `elem` s = Alts $ parseRule <$> splitOn "|" s
            | otherwise = Refs $ words s

matches :: Map String Rule -> Rule -> String -> [String]
matches rules rule s = case rule of
  Literal c -> if take 1 s == [c] then [drop 1 s] else []
  Alts alts -> do
    rule <- alts
    matches rules rule s
  Refs [] -> [s]
  Refs (ruleN:ruleNs) ->
    let
      Just rule = Map.lookup ruleN rules
      rs = matches rules rule s
    in do
        r <- rs
        matches rules (Refs ruleNs) r


matchesFull :: Map String Rule -> String -> Bool
matchesFull rules s = "" `elem` matches rules (Refs ["0"]) s

main :: IO ()
main = do
  input :: [String] <- lines <$> readFile("input.txt")
  let [rules', strings] = splitOn [""] input
  let rules = Map.fromList $ ((\[n, s] -> (n, parseRule s)) . splitOn ":") <$> rules'
  putStrLn "Part 1:"
  print $ length $ filter (\s -> matchesFull rules s) strings

  putStrLn "Part 2:"
  let rules2 = Map.insert "8" (parseRule "42 | 42 8") $ Map.insert "11" (parseRule "42 31 | 42 11 31") rules
  print $ length $ filter (\s -> matchesFull rules2 s) strings
