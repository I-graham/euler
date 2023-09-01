module P173 where

import Common (countWith)

main = print $ sols (10^6)

sols :: (Num a2, Integral a1) => a1 -> a2
sols t = sum $ map (ctWMax t) [3 .. (t`div`4)+1]

ctWMax :: (Ord t, Num p, Num t) => t -> t -> p
ctWMax t m
  | m < 3 || t < r = 0
  | otherwise = 1 + ctWMax (t - r) (m - 2)
  where
    r = ring m

ring :: Num a => a -> a
ring n = 4 * (n - 1)