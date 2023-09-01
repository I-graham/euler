module P87 where

import Common (countWith, intKthRt, intSqrt, isPrime, primesTo, sortNub)

main :: IO ()
main = do
  let limit = 5 * 10 ^ 7
  print $ length (genTriples limit)

genTriples :: Integer -> [Integer]
genTriples bound = sortNub [x + y + z | x <- sqs, y <- bnd x cbs, z <- bnd (x + y) qts]
  where
    qts = [p ^ 4 | p <- primesTo (intKthRt 4 bound)]
    cbs = [p ^ 3 | p <- primesTo (intKthRt 3 bound)]
    sqs = [p ^ 2 | p <- primesTo (intSqrt bound)]
    bnd n = takeWhile (<= bound - n)
