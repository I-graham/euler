module P343 where

import Common ( primeFactors )

main = print $ 1 + sum (map c [2..2 * 10^6])

c k = max a b
  where a = f k
        b = f (k^2-k)

f :: Integer -> Integer
f 1 = 1
f k = last (primeFactors (k+1)) - 1
