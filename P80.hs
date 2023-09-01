module P80 where

import Common (digLen, digSum, digs, intSqrt)

main :: IO ()
main = do
  let imperfects = [(100 - digLen (intSqrt n), n) | n <- [1 .. 100], n /= intSqrt n ^ 2]
  print $ sum $ map (\(l, n) -> digSum (intSqrt (n * 10 ^ (2 * l)))) imperfects
