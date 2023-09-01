module P101 where

import Common ( slice, combos )
import Data.List ( delete )

main :: IO ()
main = do
  let poly = take 11 $ iterate (* (-1)) 1 :: [Double]
  let len = fromIntegral (length poly) :: Double
  let ys = map (eval poly) [1 .. len]
  let op k = lagrange $ zip [1 ..] (slice 0 k ys)
  let funcs = map op [1 .. length poly - 1]
  let seqs = map (\f -> map (eval f) [1 .. len]) funcs
  let fits = zipWith (!!) seqs [1 ..]
  print $ sum $ map round fits

lagrange :: (Fractional a, Eq a) => [(a, a)] -> [a]
lagrange ps = do
  let (xs, ys) = unzip ps
  let polys = map (\x -> fromRoots (delete x xs)) xs
  let polysAt = zipWith eval polys xs
  let scaled = zipWith scale (map (1 /) polysAt) polys
  let parts = zipWith scale ys scaled
  foldl1 add parts

add :: Num a => [a] -> [a] -> [a]
add = zipWith (+)

scale :: Num b => b -> [b] -> [b]
scale k = map (* k)

eval :: Num a => [a] -> a -> a
eval as x = sum $ zipWith (\k a -> a * x ^ k) [0 ..] as

fromRoots :: Num a => [a] -> [a]
fromRoots rs = zipWith (\a k -> a * (-1) ^ k) mags [len, len -1 ..]
  where
    mags = map (sum . map product . combos rs) [len, len -1 .. 1] ++ [1]
    len = length rs