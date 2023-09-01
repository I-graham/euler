module P124 where

import Common ( primeFactors, rad)
import Data.List

main :: IO ()
main = do
  let ns = sortOn rad [2..100000]
  print $ ns !! (10000-2)