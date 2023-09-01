module P207 where

import Common
import Data.Ratio

main = do
  let n = head [c | c <- [1 ..], (intLogBase 2 (c + 1) % c) < 1 % 12345]
  print (n ^ 2 + n)