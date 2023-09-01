module P116 where

main :: IO ()
main = do
  let n = 50
  print $ sum $ map (`tiles` n) [2, 3, 4]

tiles :: Int -> Int -> Integer
tiles n g = aux g - 1
  where
    aux :: Int -> Integer
    aux = (map recurse [0 ..] !!)

    recurse :: Int -> Integer
    recurse g
      | g < n = 1
      | g == n = 2
      | otherwise = aux (g - 1) + aux (g - n)
