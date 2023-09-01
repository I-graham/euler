module P70 where

import Common (digLen, digs, fl, totient)
import Data.Function (on)
import Data.List (delete, minimumBy)

main :: IO ()
main = do
  let nums = [(n, ((/) `on` fl) n (totient n)) | n <- [2 .. (10 ^ 7)], digsPermute n (totient n)]
  print $ fst $ minimumBy (compare `on` snd) nums

digsPermute :: Integer -> Integer -> Bool
digsPermute p q
  | digLen p /= digLen q = False
  | otherwise = aux (digs p) (digs q)
  where
    aux [] [] = True
    aux [] qs = False
    aux (p : ps) qs = aux ps (delete p qs)