module P191 where

main :: IO ()
main = print $ sols 31

sols :: Int -> Integer
sols = (map aux [0 ..] !!)
  where
    aux n = sum (map (lateless . (n -)) [1 .. min 3 n]) + sum (map (sols . (n -)) [1 .. min 3 n])

lateless :: Int -> Integer
lateless = (map aux [0 ..] !!)
  where
    aux 0 = 1
    aux n = sum $ map (lateless . (n -)) [1 .. min 3 n]