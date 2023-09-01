module P118 where

import Common
import Data.List (permutations)

main = print $ sum $ map (sols 0) (permutations [1 .. 9])

sols :: Num p => Integer -> [Integer] -> p
sols m [] = 1
sols m ls = sum $ map (uncurry sols) $ filter check $ map col (parts ls)
  where
    col (a, b) = (fromDigs a, b)
    check (a, b) = m < a && isPrime a

parts :: [a] -> [([a], [a])]
parts l = map (`splitAt` l) [1 .. length l]