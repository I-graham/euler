module P99 where

import Common (chunk, countWith, fl, groupToPairs, wordsBy)
import Data.Function (on)
import Data.List (maximumBy)

main :: IO ()
main = do
  inp <- readFile "data/p099_base_exp.txt"
  let nums = groupToPairs (map read $ wordsBy (`elem` ",\n") inp :: [Int])
  let logs = zip [1 ..] $ map (\(b, e) -> fl e * log (fl b)) nums
  print $ fst $ maximumBy (compare `on` snd) logs