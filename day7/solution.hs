import Data.List.Split (splitOn)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- filter (not . null) <$> splitOn "\n" <$> readFile("input.txt")
  let rules = parseRule <$> input

  putStrLn "Part 1:"
  putStrLn $ show $ length $ containersOf' rules "shiny gold"

  putStrLn "Part 2:"
  putStrLn $ show $ contains' rules "shiny gold"

type Rule = (String, [(Int, String)])

parseRule :: String -> Rule
parseRule s = (unwords $ take 2 $ words container, parseContained contained)
  where
    [container, contained] = splitOn "contain" s

parseContained :: String -> [(Int, String)]
parseContained " no other bags." = []
parseContained s = parseContainedBag <$> splitOn "," s

parseContainedBag :: String -> (Int, String)
parseContainedBag s = (read count, unwords [adj, clr])
  where
    [count, adj, clr] = take 3 $ words s

containersOf :: [Rule] -> String -> [String]
containersOf rules bag = fst <$> filter (\(container, contains) -> any (\(_, bag') -> bag == bag') contains) rules

containersOf' :: [Rule] -> String -> [String]
containersOf' rules bag = nub $ do
  container <- containersOf rules bag
  container : (containersOf' rules container)

contains :: [Rule] -> String -> [(Int, String)]
contains rules bag = fromMaybe [] $ bag `lookup` rules

contains' :: [Rule] -> String -> Int
contains' rules bag = foldl (+) 0 $ do
  (count, contained) <- contains rules bag
  pure $ count + count * contains' rules contained
