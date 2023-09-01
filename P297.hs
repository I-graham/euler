module P297 where

import Common

main :: IO ()
main = print $ sTo (10 ^ 17)

sTo :: Integer -> Integer
sTo 1 = 1
sTo m
  | f == m = map aux [0 ..] !! i
  | otherwise = d + sTo d + sTo f
  where
    aux n = z (fib n - 1) + sTo (fib n - 1)

    i = hiFib m
    f = fib i
    d = m - f

z :: Integer -> Integer
z n
  | f == n = 1
  | otherwise = 1 + z (n - f)
  where
    f = fib (hiFib n)
