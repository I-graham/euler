module P135 where

import Common (asPrimes, countWith, factors)

main :: IO ()
main = print $ length [asPrimes n | n <- [1 .. 10 ^ 6], ariths n == 10]

ariths :: Integer -> Int
ariths n = countWith (isSol n) (factors n)

isSol :: Integral a => a -> a -> Bool
isSol n f = x > 0 && n == (3 * d - x) * f
  where
    d = (f + (n `div` f)) `div` 4
    x = f - d