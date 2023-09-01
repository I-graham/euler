module Common where

import           Data.Array
import           Data.Array.IO
import           Data.Function (on)
import           Data.List     as L (delete, find, group, insert, sort)
import           Data.Map      as M (Map, empty, insert, lookup, member)
import           Data.Maybe    (fromJust)
import           Data.Ratio    (denominator, numerator, (%))
import qualified Data.Set      as PQ

hiFib :: Integer -> Integer
hiFib n = head [x | x <- [0 ..], fib (x + 1) > n]

fib :: Integer -> Integer
fib = (map aux [0 ..] !!) . fromIntegral
  where
    aux 0 = 0
    aux 1 = 1
    aux n = fib (n - 1) + fib (n - 2)

psolve :: (Double -> Double) -> (Double -> Double) -> Double
psolve p d = psolve' p d 1
  where
    psolve' p d g
      | g == g' = g
      | otherwise = psolve' p d g'
      where
        g' = g - (p g / d g)

maxsub :: (Ord t, Num t) => [t] -> t
maxsub xs = aux xs 0 0
  where
    aux [] m _ = m
    aux (x : xs) m c =
      let nc = max (c + x) 0
       in aux xs (max m nc) nc

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

replace :: [a] -> Int -> a -> [a]
replace xs i x = a ++ x : tail b
  where
    (a, b) = splitAt i xs

combos :: [a] -> Int -> [[a]]
combos [] len = []
combos (n : ns) len
  | 1 + length ns < len = []
  | len == 1 = map (: []) (n : ns)
  | otherwise = map (n :) (combos ns (len - 1)) ++ combos ns len

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (l : ls)
  | l == head ls = uniq ls
  | otherwise = l : uniq ls

sortNub :: Integral a => [a] -> [a]
sortNub = uniq . sort

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsBy p s''
    where
      (w, s'') = break p s'

groupToPairs :: [a] -> [(a, a)]
groupToPairs [] = []
groupToPairs l  = (head l, l !! 1) : groupToPairs (drop 2 l)

nPk :: (Eq t, Num t, Num p) => p -> t -> p
nPk n k
  | k == 0 = 1
  | otherwise = n * nPk (n - 1) (k - 1)

nCk :: Integral p => p -> p -> p
nCk n k = nPk n k `div` fact k

primesBtw :: Integer -> Integer -> [Integer]
primesBtw l h = takeWhile (<= h) (dropWhile (< l) primes)

primesTo :: Integer -> [Integer]
primesTo n = takeWhile (<= n) primes

nthprime :: Int -> Integer
nthprime n = primes !! (n - 1)

isPrime :: Integer -> Bool
isPrime n
  | n `elem` [0, 1] = False
  | otherwise = all (\p -> n `mod` p /= 0) $ primesTo (intSqrt n)

primes :: [Integer]
primes = 2 : sieve [3, 5 ..]
  where
    sieve (x : xs) = x : sieve' xs (insertprime x xs PQ.empty)
    sieve _        = error "!"

    sieve' (x : xs) table
      | nextComposite == x = sieve' xs (adjust x table)
      | otherwise = x : sieve' xs (insertprime x xs table)
      where
        (nextComposite, _) = PQ.findMin table
    sieve' _ _ = error "!"

    adjust x table
      | n == x = adjust x (PQ.insert (n', ns) newPQ)
      | otherwise = table
      where
        Just ((n, n' : ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p * p, map (* p) xs)

digSum :: Integer -> Integer
digSum = sum . digs

digLen :: Integer -> Int
digLen = length . digs

digs :: Integral a => a -> [a]
digs 0 = []
digs n = digs (n `div` 10) ++ [n `mod` 10]

fromDigs :: Integral a => [a] -> a
fromDigs = aux . reverse
  where
    aux []       = 0
    aux (d : ds) = 10 * aux ds + d

ithdig :: (Integral t1, Num t2, Eq t2) => t1 -> t2 -> t1
ithdig n i
  | i == 0 = n `mod` 10
  | otherwise = ithdig (n `div` 10) (i - 1)

intLogBase :: (Integral a1, Num a2, Ord a2) => a2 -> a2 -> a1
intLogBase b 1 = 0
intLogBase b n = head (dropWhile ((<= n) . (b ^)) [0 ..]) - 1

isSq :: Integral a => a -> Bool
isSq n = n == rt * rt
  where
    rt = intSqrt n

intSqrt :: Integral a => a -> a
intSqrt = intKthRt 2

intKthRt :: Integral a => a -> a -> a
intKthRt k 0 = 0
intKthRt k n = intKthRt' 1 1
  where
    intKthRt' l g
      | g ^ k <= n = if (g + 1) ^ k > n then g else intKthRt' (g + 1) (2 * g)
      | otherwise = intKthRt' l ((l + g) `div` 2)

fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact n = n * fact (n - 1)

countWith :: (a -> Bool) -> [a] -> Int
countWith f = length . filter f

ang :: (Int, Int) -> Float
ang = uncurry (flip atan2 `on` fl)

fl :: Integral a => a -> Float
fl n = fromIntegral n :: Float

dropEnd :: Int -> [a] -> [a]
dropEnd n l = take (length l - n) l

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l  = take n l : chunk n (drop n l)

invAng :: Float -> Float
invAng a = normAng (a + pi)

normAng :: Float -> Float
normAng a
  | a < 0 = normAng (a + 2 * pi)
  | a > 2 * pi = normAng (a - 2 * pi)
  | otherwise = a

ccwAngles :: Ord a => a -> a -> a -> Bool
ccwAngles a b c = if c > a then c > b && b > a else (b < c) || (b > a)

type PhiCache = M.Map (Integer, Int) Integer

pCount :: Integer -> Integer
pCount n = snd (pCount' empty n)
  where
    pCount' :: PhiCache -> Integer -> (PhiCache, Integer)
    pCount' cache n
      | n < 100 = (cache, fromIntegral (length $ primesTo n))
      | otherwise = do
        let (cache', a) = pCount' cache (intSqrt n)
        let (cache'', phival) = phi cache' n (fromIntegral a)
        (cache, phival + a - 1)

    phi :: PhiCache -> Integer -> Int -> (PhiCache, Integer)
    phi cache m n
      | 0 == m || n == 0 = (cache, m)
      | n == 1 = (cache, (m + 1) `div` 2)
      | m < nthprime n = (cache, 1)
      | member (m, n) cache = (cache, fromJust (M.lookup (m, n) cache))
      | otherwise = do
        let (astr, a) = phi cache m (n - 1)
        let cache' = if m > nthprime (n + 1) then M.insert (m, n - 1) a astr else astr

        let newM = m `div` nthprime n

        let (bstr, b) = phi cache' newM (n - 1)
        let cache'' = if newM > nthprime (n + 1) then M.insert (newM, n - 1) b bstr else bstr
        (cache'', a - b)

removeFactor :: Integral a => a -> a -> a
removeFactor f n = head $ dropWhile ((== 0) . (`mod` f)) $ iterate (`div` f) n

rad :: Integer -> Integer
rad n = product $ primeFactors n

asPrimes :: Integer -> [Integer]
asPrimes n = aux ps n
  where
    ps = [p | p <- primesTo (intSqrt n), n `mod` p == 0]
    aux _ 1 = []
    aux [] n = [n]
    aux (p:pt) n
      | p*p > n = [n]
      | n `mod` p == 0 = p : aux (p:pt) (n `div` p)
      | otherwise = aux pt n

primeFactors :: Integer -> [Integer]
primeFactors n = aux ps n
  where
    ps = [p | p <- primesTo (intSqrt n)]
    aux _ 1      = []
    aux [] n     = [n]
    aux (p:pt) n
      | p*p > n = [n]
      | n `mod` p == 0 = p : t
      | otherwise = t
      where t = aux pt (removeFactor p n)

powers :: [Integer]
powers = uniq $ concatMap (\x -> x ^ 2 : aux 3 (x ^ 2) ((x + 1) ^ 2)) [1 ..]
  where
    aux :: Integral a => a -> a -> a -> [a]
    aux k lo hi
      | lort == hirt = []
      | otherwise = map (^ k) [lort + 1 .. hirt] ++ aux (k + 1) lo hi
      where
        lort = intKthRt k lo
        hirt = intKthRt k hi

totient :: Integer -> Integer
totient 1 = 1
totient n = numerator ratio `div` denominator ratio
  where
    ratio =
      foldl
        (\acc x -> acc * (1 - (1 % x)))
        (n % 1)
        (primeFactors n)


pExpForm :: [Integer] -> [(Integer, Integer)]
pExpForm ps = [(head x, fromIntegral (length x)) | x <- (group . sort) ps]

prodsOfPs :: [Integer] -> [Integer]
prodsOfPs = prods . pExpForm
  where
    prods :: [(Integer, Integer)] -> [Integer]
    prods []            = [1]
    prods ((p, c) : xs) = concat [map (* p^n) base | n <- [0..c]] where base = prods xs

ioGenByPs :: Integer -> IO (Array Integer [Integer])
ioGenByPs n = sieve ps >>= freeze
  where
    ps = primesTo n

    sieve :: [Integer] -> IO (IOArray Integer [Integer])
    sieve []     = newArray (1,n) []
    sieve (p:ps) = do
      arr <- sieve ps
      let ns = [n | k <- [1..intLogBase p n], n <- [p^k, 2*p^k..n]]
      ps <- sequence [ readArray arr n | n <- ns]
      sequence_ [writeArray arr n (p:l) | (n, l) <- zip ns ps]
      return arr

generateByPs :: Integer -> [[Integer]]
generateByPs n = aux [] smallps n ++ map (:[]) rest
  where
    (smallps, rest) = span (<= (n `div` 2) ) (primesTo n)
    aux acc (p:ps) n
      | p > n = [acc]
      | otherwise = aux (p:acc) (p:ps) (n `div` p) ++ aux acc ps n
    aux acc [] _ = [acc]

factors :: Integer -> [Integer]
factors n = sort $ prodsOfPs (asPrimes n)
