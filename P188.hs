module P188 where

import Common ( totient )

main :: IO ()
main = print $ hyp (10 ^ 8) 1777 1855

hyp :: (Eq t, Num t) => Integer -> Integer -> t -> Integer
hyp n a 0 = 1
hyp 1 a k = 1
hyp n a k = (a ^ hyp (totient n) a (k - 1)) `mod` n
