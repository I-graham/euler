module P601 where

main = print $ sum $ map (\i -> p i (4^i)) [1..31]

p i n = p' i n - p' (i+1) n

p' i n = (n-2) `div` foldr lcm 1 [1..i]
