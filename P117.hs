module P117 where

main :: IO ()
main = print $ tiles 50

options :: [Int]
options = [1..4]

tiles :: Int -> Integer
tiles = (map aux [0 ..] !!)
  where
    aux :: Int -> Integer
    aux n
      | n == 0 = 1
      | otherwise = sum $ map tiles $ filter (>= 0) $ map (n -) options