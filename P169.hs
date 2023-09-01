module P169 where

import Common
import Data.Map

main = print $ f (10 ^ 25)

f n = snd (ways empty (intLogBase 2 n) n)

ways cache m n
  | m < 0 || n < 0 = (cache, 0)
  | n >= 2 ^ (m + 2) = (cache, 0)
  | m == 0 && n <= 2 = (cache, 1)
  | n == 0 = (cache, 1)
  | member (m, n) cache = (cache, cache ! (m, n))
  | otherwise = do
    let (mem, a) = ways cache (m - 1) n
        (mem', b) = ways mem (m - 1) (n - (2 ^ m))
        (mem'', c) = ways mem' (m - 1) (n - (2 ^ (m + 1)))
        res = a + b + c
    (insert (m, n) res mem'', res)