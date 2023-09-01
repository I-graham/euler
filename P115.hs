module P115 where

m = 50

main = print $ head (filter ((> 1000000) . ways) [1 ..]) - 1

ways = (map aux [0 ..] !!)
  where
    aux 0 = 1
    aux n = sum $ map (ways . (n -)) (1 : [(1 + m) .. n])
