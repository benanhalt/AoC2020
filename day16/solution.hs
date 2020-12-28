{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List (tails, foldl', delete, sort, isPrefixOf, partition, nub, (\\))
import Data.List.Split (splitOn)
import Control.Monad (guard)

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  let [fields, rest] = filter (not . null) <$> splitOn ["your ticket:"] input
  let [mine', nearby'] = filter (not . null) <$> splitOn ["nearby tickets:"] rest
  let mine :: [Int] = ((read <$>) . splitOn ",") $ head mine'
  let nearby :: [[Int]] = ((read <$>) . splitOn ",") <$> nearby'
  let rulesByName :: [(String, [[Int]])] = (\[name, rule] -> (name, ((fmap read) . splitOn "-") <$> splitOn " or " rule)) <$> splitOn ": " <$> fields

  putStrLn "Part 1:"
  -- putStrLn $ unlines fields
  let ranges :: [[Int]] = concat $ snd <$> rulesByName

  let invalid = filter (not . isValid ranges) $ concat nearby
  -- print invalid
  print $ sum invalid

  putStrLn "Part 2:"

  let validTickets = do
        ticket <- nearby
        guard $ all (isValid ranges) ticket
        pure $ ticket

  let nFields = length $ head validTickets

  -- print $ take 5 validTickets

  let possibilities = do
        (rule, ranges) <- rulesByName
        let fs = [0..nFields-1] \\ do
              t <- validTickets
              (f, v) <- zip [0..] t
              guard $ not $ isValid ranges v
              pure f
        pure $ (rule, fs)

  let nameToField = solve possibilities []

  -- putStrLn $ unlines $ show <$> nameToField

  let myTicket = do
        (name, [f]) <- nameToField
        pure $ (name, mine !! f)

  -- putStrLn $ unlines $ show <$> myTicket

  print $ product $ (\(name, v) -> if "departure" `isPrefixOf` name then v else 1) <$> myTicket


solve :: [(String, [Int])] -> [(String, [Int])] -> [(String, [Int])]
solve [] def = def
solve indef def =
  let
    (def', indef') = partition ((== 1) . length . snd) indef
    indef'' = do
      (rule, fs) <- indef'
      pure (rule, fs \\ ((head . snd) <$> def'))
  in
    solve indef'' (def ++ def')

isValid :: [[Int]] -> Int -> Bool
isValid ranges n = or $ do
  [min', max'] <- ranges
  pure $ n >= min' && n <= max'

