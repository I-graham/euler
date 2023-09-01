module P225 where

import Data.Set (Set, empty, insert, member)

main = print $ [n | n <- [1, 3 ..], f n] !! 123

f n = f' n empty 1 1 1

f' :: Integral t => t -> Set [t] -> t -> t -> t -> Bool
f' n cache a b c
  | c == 0 = False
  | member [a, b, c] cache = True
  | otherwise = f' n (insert [a, b, c] cache) b c ((a + b + c) `mod` n)