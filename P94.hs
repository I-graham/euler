module P94 where

import Common
import Data.Maybe (fromJust, isJust, mapMaybe)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  let bound = 10 ^ 9
  let cs = takeWhile (< bound) $ mapMaybe triIntSAc [n * n | n <- [1 ..]]
  let qs = takeWhile (< bound) $ mapMaybe triIntSAq [n * n | n <- [2 ..]]
  print $ sum (cs ++ qs)

triIntSAc :: Integer -> Maybe Integer
triIntSAc c
  | isSq (3 * c + 1) = Just (12 * c + 4)
  | otherwise = Nothing

triIntSAq :: Integer -> Maybe Integer
triIntSAq q
  | isSq (3 * q - 2) = Just (6 * q - 4)
  | otherwise = Nothing
