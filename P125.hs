module P125 where

import Common (countWith, digs, fromDigs, intSqrt, pow)
import Data.List (find)
import Data.Maybe

main :: IO ()
main = do
  let limit = 8
  let ePals = takeWhile (< pow 10 limit) [makeEPal n | n <- [1 ..]]
  let oPals = takeWhile (< pow 10 limit) [makeOPal n | n <- [1 ..]]
  print $ sum $ filter sumOfSquares (oPals ++ ePals)

makeEPal :: Integral a => a -> a
makeEPal n = fromDigs (ds ++ reverse ds)
  where
    ds = digs n

makeOPal :: Integral a => a -> a
makeOPal n = fromDigs (init ds ++ reverse ds)
  where
    ds = digs n

sumOfSquares :: Integral a => a -> Bool
sumOfSquares n
  | n == pow root 2 = False
  | otherwise = any (aux n) [root, root - 1 .. 1]
  where
    root = intSqrt n

    aux n hi
      | hi == 0 = False
      | n < square = False
      | square == n = True
      | otherwise = aux (n - square) (hi - 1)
      where
        square = hi * hi