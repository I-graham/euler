module P77 where

import Common (primesBtw)
import Data.List (find)

main :: IO ()
main = print $ find ((> 5000) . waysToSum) [1 ..]

waysToSum :: Integer -> Integer
waysToSum = waysToSum' 2

waysToSum' :: Integer -> Integer -> Integer
waysToSum' l n
  | n < l = 0
  | otherwise = (if n == last primes then 1 else 0) + sum (map (\p -> waysToSum' p (n - p)) primes)
  where
    primes = primesBtw l n
