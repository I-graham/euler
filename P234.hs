module P234 where

import Common

lim = 999966663333

main = print $ sumSemis lim

sumSemis :: Integer -> Integer
sumSemis n = sum (map segment ps)
  where
    psTo = primesTo (intSqrt n)
    ps = zip psTo (tail psTo)
    segment (l, h) = do
      let d = h ^ 2 - l ^ 2
      let ls = d `div` l
      let hs = d `div` h
      l * (l * ls + sumTo ls) + h * (h * hs - sumTo hs) - 2 * l * h

sumTo k = k * (k + 1) `div` 2