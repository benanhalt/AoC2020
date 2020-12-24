{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.Char (isDigit, digitToInt, isAlpha)
import Data.List (tails, foldl', delete, sort, isPrefixOf, intercalate, partition, intersect, nub, (\\))
import Data.Maybe (maybeToList)
import Data.List.Split (splitOn)
import Control.Monad (guard)
import Debug.Trace (traceShowId, traceShow)
import Data.Function (fix)

type Contains = ([String], [String])

parseL :: String -> Contains
parseL l = (ingredients, allergens)
  where
    [ingredients, allergens] = splitOn ["contains"] $ filter isAlpha <$> words l


main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")

  putStrLn "Part 1:"
  let parsed = parseL <$> input
  let allA = nub $ concat $ snd <$> parsed
  let allI = nub $ concat $ fst <$> parsed

  let notIn = do
        a <- allA
        let notInI = do
              (is, as) <- parsed
              guard $ elem a as
              allI \\ is
        pure (a, nub notInI)

  let inI = do
        (a, is) <- notIn
        pure (a, allI \\ is)

  let ais = solve inI []
  let bad = snd <$> ais
  let good = allI \\ bad
  print $ sum $ do
    (is, _) <- parsed
    pure $ length $ filter (`elem` good) is

  putStrLn "Part 2:"
  putStrLn $ intercalate "," $ snd <$> sort ais


solve :: [(String, [String])] -> [(String, String)] -> [(String, String)]
solve [] answers = answers
solve pos ans =
  let
    (def, indef) = partition (\(a, is) -> 1 == length is) pos

    ans' = ans ++ (do
                      (a, [i]) <- def
                      pure (a, i)
                  )
    indef' = (do
                 (a, is) <- indef
                 let cantBe = snd <$> ans'
                 pure (a, is \\ cantBe)
             )
  in
    solve indef' ans'
