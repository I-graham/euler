module P123 where

import Common

main :: IO ()
main = print $ head [n | n <- [5, 7 ..], r n > (10 ^ 10) `div` 2]

r :: Int -> Integer
r n = p * (fromIntegral n `mod` p)
  where
    p = primes !! (n - 1)