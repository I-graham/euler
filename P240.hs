module P240 where

import Data.List

n = 6

countWithSum l 0 = zeros l 
countWithSum l s = vSum [vOneAt i `vAdd` | i <- [1..n-1]]

vOneAt i
  | i < n = zeros i ++ 1:zeros (n-i-1)
  | otherwise = vZero

vAdd = zipWith (+) 

vSum = foldl1' vAdd

zeros = flip replicate 0