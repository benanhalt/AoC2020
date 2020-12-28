{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
  input <- lines <$> readFile("input.txt")
  putStrLn "Part 1:"
  -- putStrLn $ unlines input
  let results = (evalFull L2R) <$> input
  -- putStrLn $ unlines $ show <$> results
  print $ sum $ fst <$> results

  putStrLn "Part 2:"
  -- putStrLn $ unlines input
  let results = (evalFull AddFirst) <$> input
  -- putStrLn $ unlines $ show <$> results
  print $ sum $ fst <$> results

data Mode = AddFirst | L2R

data StackElem
  = Value Int
  | OpPlus
  | OpMult
  deriving (Eq, Show)

eval :: Mode -> String -> [StackElem] -> ([StackElem], String)
eval _ [] stack = (stack, [])
eval mode (c: rest) stack = case c of
  ' ' -> eval mode rest stack
  '+' -> eval mode rest (OpPlus:stack)
  '*' -> eval mode rest (OpMult:stack)
  '(' -> let (v, rest') = evalFull mode rest in pushValue mode v rest' stack
  ')' -> (stack, rest)
  otherwise -> if isDigit c then pushValue mode (digitToInt c) rest stack else error $ "bad char: " ++ [c]

pushValue :: Mode -> Int -> String -> [StackElem] -> ([StackElem], String)
pushValue mode v rest stack =
  case stack of
    [] -> eval mode rest (Value v:stack)
    (OpPlus:Value d:stack) -> eval mode rest ((Value $ v + d) : stack)
    (OpMult:Value d:stack) -> case mode of
                                L2R -> eval mode rest ((Value $ v * d) : stack)
                                AddFirst -> eval mode rest (Value v:OpMult:Value d:stack)
    _ -> error $ "bad stack: " ++ show stack

evalFull :: Mode -> String -> (Int, String)
evalFull mode s = let (stack, rest) = eval mode s [] in (reduce stack, rest)
  where
    reduce :: [StackElem] -> Int
    reduce [Value v] = v
    reduce (Value v : OpPlus : stack) = v + reduce stack
    reduce (Value v : OpMult : stack) = v * reduce stack
    reduce s = error $ "bad evalFull: " ++ show s

