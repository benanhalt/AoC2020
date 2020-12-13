{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List (sortOn)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  [depart', buses'] <- lines <$> readFile("input.txt")
  let buses = splitOn "," buses'
  let depart :: Int = read depart'

  putStrLn "Part 1:"
  let operating :: [Int] = read <$> (filter (/= "x") buses)
  let (bus, wait) = head $ sortOn snd $ (\b -> (b, b - (depart `mod` b))) <$> operating
  putStrLn $ show $ bus * wait

  putStrLn "Part 2:"
  let pairs = (\(d, b') -> let b = read b' in (b-d, b)) <$> (filter ((/= "x") . snd) $ zip [0..] buses)
  putStrLn $ show $ fst $ chinese pairs

inverse :: Int -> Int -> Int
inverse a n = inverseR 0 1 n a
  where
    inverseR t t' r 0 = if r > 1 then error "not invertible" else t
    inverseR t t' r r' = let q = r `div` r' in inverseR t' (t - q*t') r' (r - q*r')

chinese :: [(Int, Int)] -> (Int, Int)
chinese pairs =
  let
    bigN = product $ snd <$> pairs
    x = sum $ do
      (b, n) <- pairs
      let n' = bigN `div` n
      let x = inverse n' n
      pure $ b * n' * x
  in
    (x `mod` bigN, bigN)
