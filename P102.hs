module P102 where

import Common (ang, ccwAngles, chunk, countWith, groupToPairs, invAng, normAng, wordsBy)
import Data.List (sort)

main :: IO ()
main = do
  tris <- readFile "data/p102_triangles.txt"
  let nums = chunk 3 $ groupToPairs (map read $ wordsBy (`elem` ",\n") tris :: [Int])
  print $ countWith containsOrigin nums

containsOrigin :: [(Int, Int)] -> Bool
containsOrigin ps = do
  let [a, b, c] = sort $ map (normAng . ang) ps
  ccwAngles (invAng b) a (invAng c)
    && ccwAngles (invAng c) b (invAng a)
    && ccwAngles (invAng a) c (invAng b)