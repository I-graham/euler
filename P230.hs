module P230 where

import Common

a :: [Integer]
a = digs 1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679

b :: [Integer]
b = digs 8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196

main = print $ sum [10 ^ n * d ((127 + 19 * n) * 7 ^ n - 1) | n <- [0 .. 17]]

d :: Integer -> Integer
d n = ([a, b] !! fromInteger (p q)) !! fromInteger r
  where
    len = fromIntegral (length a)
    q = n `div` len
    r = n `mod` len

p :: Integer -> Integer
p 1 = 1
p n = p' (1 + hiFib n) n

p' :: Integer -> Integer -> Integer
p' l 0 = l `mod` 2
p' l n
  | n < f = p' (l - 2) n
  | otherwise = p' (l - 1) (n - f)
  where
    f = fib (l - 1)