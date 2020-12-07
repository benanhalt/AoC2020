import Control.Monad (guard)

main :: IO ()
main = do
  inputStrings <- words <$> readFile("input.txt")
  let input = read <$> inputStrings
  let result = do
        a <- input
        b <- input
        guard $ a + b == 2020
        pure $ ((a,b), a * b)

  putStrLn "Part 1:"
  putStrLn $ show $ head result

  let result2 = do
        a <- input
        b <- input
        guard $ a + b <= 2020
        c <- input
        guard $ a + b + c == 2020
        pure $ ((a,b,c), a * b * c)

  putStrLn "Part 2:"
  putStrLn $ show $ head result2

