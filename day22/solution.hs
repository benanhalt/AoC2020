{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List.Split (splitOn)

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  let decks :: [[Int]] = (read <$>) <$> drop 1 <$> splitOn [""] input

  putStrLn "Part 1:"
  print $ score <$> play decks

  putStrLn "Part 2:"
  print $ score <$> playRC Set.empty decks

-- Score a Combat deck.
score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

-- Play a game of Combat.
play :: [[Int]] -> [[Int]]
play decks@[[], _] = decks -- player two has won
play decks@[_, []] = decks -- player one has won
play decks = play $ playRound decks -- play a round and continue

-- Play a round of Combat.
playRound :: [[Int]] -> [[Int]]
playRound [(c1:d1), (c2:d2)] | c1 > c2 = [d1 ++ [c1, c2], d2] -- player one wins
                             | c2 > c1 = [d1, d2 ++ [c2, c1]] -- player two wins
                             | otherwise = error "same card"

-- Play a game of Recursive Combat.
playRC :: Set [[Int]] -> [[Int]] -> [[Int]]
playRC _ decks@[[], _] = decks -- player two has won
playRC _ decks@[_, []] = decks -- player one has won

playRC seen decks@[d1, d2] | decks `Set.member` seen = [d1, []] -- previously seen deck configuration -> player one wins

playRC seen decks@[(c1:d1), (c2:d2)] | length d1 < c1 || length d2 < c2 = playRC seen' $ playRound decks -- not enough cards to recurse

                                     | otherwise = if playerOneWinsSubGame [take c1 d1, take c2 d2] -- play subgame
                                                   then
                                                     playRC seen' [d1 ++ [c1, c2], d2] -- player one won subgame
                                                   else
                                                     playRC seen' [d1, d2 ++ [c2, c1]] -- player two won subgame
  where
    seen' = (decks `Set.insert` seen)

-- Play a game of Recursive Combat and return whether player one wins.
playerOneWinsSubGame :: [[Int]] -> Bool
playerOneWinsSubGame decks =
  let
    [d1, d2] = playRC Set.empty decks
  in
    not $ null $ d1 -- did player one win?
