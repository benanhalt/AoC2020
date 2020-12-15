{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Data.List.Split (splitOn)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let input :: [Int] = read <$> splitOn "," "17,1,3,16,19,0"
  let game' len = game len (length input) (Map.fromList $ zip (init input) [0..]) $ last input

  putStrLn "Part 1:"
  print $ game' 2020

  putStrLn "Part 2:"
  print $ game' $ 30000000

game :: Int -> Int -> Map Int Int -> Int -> Int
game end count m prev = if count == end then prev else recurse
  where
    recurse = game end (count + 1) (Map.insert prev (count - 1) m) next
    next = case Map.lookup prev m of
      Nothing -> 0
      Just j -> count - j - 1
