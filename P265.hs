module P265 where

import Common
import Data.Array
import Data.List
import Debug.Trace

main = print $ sum (map fromBin (solve 5))

solve :: (Integral a, Ix a) => Int -> [[a]]
solve n = s arr ones zeros init
  where
    arr = listArray (0, 2 ^ n - 1) (True : repeat False)
    half = 2 ^ (n - 1)
    ones = half - countWith (== 1) init
    zeros = half - countWith (== 0) init
    init = replicate n 0
    inits = [0]

    s arr os zs l
      | os == 0 && zs == 0 =
        let cands = map (toBin . fst) $ filter (not . snd) (assocs arr)
         in [l | all (`isInfixOf` (l ++ l)) cands]
      | os == 0 = addZero
      | zs == 0 = addOne
      | otherwise = addZero ++ addOne
      where
        onl = 1 : l
        offl = 0 : l
        on = fromBin (take n onl)
        off = fromBin (take n offl)
        addOne = if arr ! on then [] else s (arr // [(on, True)]) (os - 1) zs onl
        addZero = if arr ! off then [] else s (arr // [(off, True)]) os (zs - 1) offl

fromBin [] = 0
fromBin (l : ls) = l + 2 * fromBin ls

toBin 0 = []
toBin n = (n `mod` 2) : toBin (n `div` 2)
