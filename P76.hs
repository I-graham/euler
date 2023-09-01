module P76 where

import Common (primes)
import Data.List (find)
import Debug.Trace

main :: IO ()
main = print $ waysToSum 100

waysToSum :: Integer -> Integer
waysToSum n = waysToSum' 1 n - 1

waysToSum' :: Integer -> Integer -> Integer
waysToSum' l n
  | n == 0 || n == l = 1
  | n < l = 0
  | otherwise = sum $ map (\c -> waysToSum' c (n - c)) [l .. n]
