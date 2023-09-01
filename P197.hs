module P197 where

import Common

main = do
  let alternating = dropWhile (\x -> x /= (f . f) x) $ iterate f (-1)
  print $ sum $ take 2 alternating

c :: Double
c = 30.403243784

f :: Double -> Double
f x = 10 ** (-9) * fromInteger (floor (2 ** (c - x ** 2)))