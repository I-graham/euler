module P74 where

import Common (countWith, digs, fact)

main :: IO ()
main = do
  let digs = map (chlen []) [1 .. 1000000]
  print $ countWith (== 60) digs

chlen :: (Integral t, Num p) => [t] -> t -> p
chlen seen n
  | n `elem` seen = 0
  | otherwise = 1 + chlen (n : seen) (cyc n)

cyc :: Integral a => a -> a
cyc n = sum $ map fact (digs n)