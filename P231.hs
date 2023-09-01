module P231 where

import Common

main = print $ calc (2 * 10 ^ 7) (5 * 10 ^ 6)

calc :: Integer -> Integer -> Integer
calc n k = sum [f (n + 1 - i) - f i | i <- [1 .. k]]

f 0 = 0
f 1 = 0
f n =
  let p = head (primeFactors n)
   in p + f (n `div` p)