module P206 where

import Common ( ithdig, intSqrt )

main :: IO ()
main = do
  let l = intSqrt 1020304050607080900
  let u = intSqrt 1929394959697989990
  print $ head [x | x <- [l,l+10 ..], fitsCond (x * x)]

fitsCond :: Integral a => a -> Bool
fitsCond n = ithdig n 0 == 0 && all (\i -> ithdig n (2 * i) == (10 - i)) [1 .. 10]