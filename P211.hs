module P211 where

import           Common

main = print $ sum [ product ps | ps <- generateByPs (64 * 10^6), sigIsSq ps]

sigIsSq :: [Integer] -> Bool
sigIsSq ps = intSqrt s ^2 == s where s = sig ps

sig :: [Integer] -> Integer
sig ps = product $ map fac pef
  where pef = pExpForm ps
        fac (p,k) = (p^(2*k+2)-1) `div` (p^2-1)


