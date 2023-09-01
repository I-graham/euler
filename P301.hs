module P301 where

main :: IO ()
main = print $ sols 31

sols :: Int -> Integer
sols = (map aux [0 ..] !!)
  where
    aux 1 = 1
    aux 2 = 2
    aux n = sols (n - 1) + sols (n - 2)