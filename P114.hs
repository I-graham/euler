module P114 where

main = print $ ways 51

ways = (map aux [0 ..] !!)
  where
    aux 0 = 1
    aux n = sum $ map (ways . (n -)) (1 : [4 .. n])