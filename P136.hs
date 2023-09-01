module P136 where

import           Common

main :: IO ()
main = print $ length [ s | ps <- generateByPs (5 * 10^7), let s = solve ps, not (null s), null (tail s)]

solve ps = [ (c,d) |  let k = product ps,
                      f <- prodsOfPs ps,
                      let c = k `div` f,
                      (c+f) `mod` 4 == 0,
                      let d = (c + f) `div` 4,
                      f > d ]
