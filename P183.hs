module P183 where

import Common
import Data.Function
import Data.Ratio
import Debug.Trace

main = print $ sum $ map d [5 .. 10000]

d :: Integer -> Integer
d n = if (t . m) n then -n else n

s :: Integer -> Integer -> Ratio Integer
s n k = (n % k) ^ k

m :: Integer -> Ratio Integer
m n = (max `on` s n) h (h + 1)
  where
    e = exp 1
    h = floor (fl n / e)

t :: Integral a => Ratio a -> Bool
t r = 1 == rem 2 (rem 5 d)
  where
    d = denominator r
    rem = removeFactor