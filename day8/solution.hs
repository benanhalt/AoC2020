{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- filter (not . null) <$> splitOn "\n" <$> readFile("input.txt")
  let prog = parseOp <$> input
  let state0 = State { pc = 0, acc = 0, pastPc = [] }

  putStrLn "Part 1:"
  putStrLn $ show $ acc $ exec' prog state0

  let modifiedProgs = (\(i, _) -> modifyProg prog i) <$> zip [0..] prog
  let allResults = (\prog' -> (prog', exec' prog' state0)) <$> modifiedProgs
  let (_, termed) = head $ filter (\(prog', state) -> hasTerminated prog' state) allResults

  putStrLn "Part 2:"
  putStrLn $ show $ acc termed

data Op
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Eq, Show)

parseOp :: String -> Op
parseOp s = case op of
  "acc" -> Acc int
  "jmp" -> Jmp int
  "nop" -> Nop int
  where
    [op, val] = words s
    int = read $ filter (/= '+') val


type Prog = [Op]

data State = State
  { pc :: Int
  , acc :: Int
  , pastPc :: [Int]
  }
  deriving (Show)

hasLooped :: State -> Bool
hasLooped State {pc, pastPc} = pc `elem` pastPc

hasTerminated :: Prog -> State -> Bool
hasTerminated prog (State {pc}) = pc >= length prog

exec :: Prog -> State -> State
exec prog s@State {pc, acc, pastPc} =
  if hasTerminated prog s then s
  else case prog !! pc of
    Acc v -> s { pastPc = pc : pastPc, pc = pc + 1, acc = acc + v }
    Jmp v -> s { pastPc = pc : pastPc, pc = pc + v }
    Nop _ -> s { pastPc = pc : pastPc, pc = pc + 1 }


modifyProg :: Prog -> Int -> Prog
modifyProg prog i =
  case prog !! i of
    Jmp v -> updateAt i (Nop v) prog
    Nop v -> updateAt i (Jmp v) prog
    _ -> prog


updateAt :: Int -> a -> [a] -> [a]
updateAt i a as = h ++ (a : (drop 1 t))
  where (h, t) = splitAt i as


exec' :: Prog -> State -> State
exec' prog state = head b
  where
    (a, b) = span (\s -> (not $ hasLooped s) && (not $ hasTerminated prog s)) $ iterate (exec prog) state
