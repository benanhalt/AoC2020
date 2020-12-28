{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Data.Maybe (maybeToList, listToMaybe)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



key1 = 9232416
key2 = 14144084

p = 20201227

main :: IO ()
main = do

  putStrLn "Part 1:"
  let Just ls1 = bsgs 7 key1 p
  let Just ls2 = bsgs 7 key2 p

  -- print $ (ls1, ls2)
  print $ fastpow key1 ls2 p
  -- print $ fastpow key2 ls1 p


-- Baby-step giant-step
-- returns x s.t. g^x = h mod p
bsgs :: Integer -> Integer -> Integer -> Maybe Integer
bsgs g h p =
  let
    m = ceiling $ sqrt $ fromIntegral p - 1
    table :: Map Integer Integer = Map.fromList $ (\i -> (fastpow g i p, i)) <$> [0..m-1]
    c = fastpow g (m * (p - 2)) p
  in
    listToMaybe $ do
    j <- [0..m-1]
    let y = (h * fastpow c j p) `mod` p
    ty <- maybeToList $ Map.lookup y table
    pure $ j * m + ty

fastpow :: Integer -> Integer -> Integer -> Integer
fastpow base exp modulo = fastpow' (base `mod` modulo) exp modulo 1
  where fastpow' b 0 m !r = r
        fastpow' b e m r = fastpow' (b * b `mod` m) (e `div` 2) m (if even e then r else (r * b `mod` m))

