module P131 where

import Common (isPrime)

main :: IO ()
main = do
  let limit = 10 ^ 6
  print $ length $ takeWhile (< limit) [p | c <- [1 ..], let p = 3 * c * (c + 1) + 1, isPrime p]