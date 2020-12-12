{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}


main :: IO ()
main = do
  input <- words <$> readFile("input.txt")

  putStrLn "Part 1:"
  let (heading, x, y) = foldl (flip step) (90, 0, 0) input
  putStrLn $ show $ (abs x) + (abs y)

  putStrLn "Part 2:"
  let (x, y, wpx, wpy) = foldl (flip step2) (0, 0, 10, 1) input
  putStrLn $ show $ (abs x) + (abs y)

step2 :: String -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
step2 (dir : amountS) (x, y, wpx, wpy) =
  case dir of
    'N' -> (x, y, wpx, wpy + amount)
    'S' -> (x, y, wpx, wpy - amount)
    'E' -> (x, y, wpx + amount, wpy)
    'W' -> (x, y, wpx - amount, wpy)
    'F' -> (x + amount * wpx, y + amount * wpy, wpx, wpy)
    'L' -> (iterate left (x, y, wpx, wpy)) !!  (amount `div` 90)
    'R' -> (iterate right (x, y, wpx, wpy)) !! (amount `div` 90)
  where
    amount :: Int = read amountS
    left (x, y, wpx, wpy) = (x, y, -wpy, wpx)
    right (x, y, wpx, wpy) = (x, y, wpy, -wpx)


step :: String -> (Int, Int, Int) -> (Int, Int, Int)
step (dir : amountS) (heading, x, y) =
  case dir of
    'F' -> step' (heading2dir heading) amount (heading, x, y)
    'L' -> ((heading - amount) `mod` 360, x, y)
    'R' -> ((heading + amount) `mod` 360, x, y)
    d -> step' d amount (heading, x, y)
  where
    amount :: Int = read amountS

step' :: Char -> Int -> (Int, Int, Int) -> (Int, Int, Int)
step' dir amount (heading, x, y) =
  case dir of
    'N' -> (heading, x, y + amount)
    'S' -> (heading, x, y - amount)
    'E' -> (heading, x + amount, y)
    'W' -> (heading, x - amount, y)

heading2dir :: Int -> Char
heading2dir 0 = 'N'
heading2dir 90 = 'E'
heading2dir 180 = 'S'
heading2dir 270 = 'W'
