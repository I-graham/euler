module P185 where

import Common
import Data.Function
import Data.List
import Data.Maybe
import Debug.Trace

main = print $ search givens' potentials'

search :: [([Int], Int)] -> [[Int]] -> Maybe [[Int]]
search known pots
  | (snd . head) known == 0 = search (tail known) (zipWith delete ((fst . head) known) pots)
  | all ((== 1) . length) pots = if null known then Just pots else Nothing
  | any null pots || null known = Nothing
  | otherwise = do
    let (next, sels) = traceShowId $ minimumBy (compare `on` length) $ filter ((> 1) . length . snd) (zip [0 ..] pots)
    let pots' = map (replace pots next . (: [])) sels
    let known' = map (\s -> sortOn snd $ map (\(d, c) -> (d, if (d !! next) == s then c - 1 else c)) known) sels
    listToMaybe $ catMaybes $ zipWith search known' pots'

potentials :: [[Int]]
potentials = replicate 16 [0 .. 9]

potentials' :: [[Int]]
potentials' = replicate 5 [0 .. 9]

givens' :: [([Int], Int)]
givens' =
  [ (digs 70794, 0),
    (digs 34109, 1),
    (digs 12531, 1),
    (digs 90342, 2),
    (digs 39458, 2),
    (digs 51545, 2)
  ]

givens :: [([Int], Int)]
givens =
  [ (digs 2321386104303845, 0),
    (digs 3847439647293047, 1),
    (digs 4895722652190306, 1),
    (digs 3174248439465858, 1),
    (digs 8157356344118483, 1),
    (digs 6913859173121360, 1),
    (digs 6375711915077050, 1),
    (digs 5616185650518293, 2),
    (digs 4513559094146117, 2),
    (digs 2615250744386899, 2),
    (digs 6442889055042768, 2),
    (digs 2326509471271448, 2),
    (digs 5251583379644322, 2),
    (digs 2659862637316867, 2),
    (digs 5855462940810587, 3),
    (digs 9742855507068353, 3),
    (digs 4296849643607543, 3),
    (digs 7890971548908067, 3),
    (digs 8690095851526254, 3),
    (digs 1748270476758276, 3),
    (digs 3041631117224635, 3),
    (digs 1841236454324589, 3)
  ]