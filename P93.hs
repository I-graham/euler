module P93 where

import Common
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Set as S
  ( Set,
    cartesianProduct,
    empty,
    fromList,
    map,
    member,
    notMember,
    powerSet,
    size,
    toList,
    union,
    unions,
    (\\),
  )
import Distribution.Simple.GHC (GhcImplInfo (alwaysNondecIndent))

main :: IO ()
main = do
  let seqs = zip [1 ..] $ Prelude.map (maxNs . producible) digSets
  let best = fst $ maximumBy (compare `on` snd) seqs
  print $ digSets !! (best - 1)

combine :: Int -> Int -> Set Int
combine a b = do
  let always = filter (> 0) [a * b, a + b, a - b, b - a]
  let divide = [a `div` b | a `mod` b == 0] ++ [b `div` a | b `mod` a == 0]
  (union `on` fromList) always divide

producible :: Set Int -> Set Int
producible ns
  | size ns <= 1 = ns
  | size ns == 2 = let [a, b] = toList ns in combine a b
  | otherwise = do
    let order l r = (min l r, max l r)
    let splits = S.map (\s -> order s (ns \\ s)) pows
    let prods = unions $ S.map (uncurry (cartesianProduct `on` producible)) splits
    unions $ S.map (uncurry combine) prods
  where
    pows = powerSet ns \\ fromList [ns, empty]

maxNs :: Set Int -> Int
maxNs set
  | notMember 1 set = 0
  | otherwise = last $ takeWhile (`member` set) [1 ..]

digSets :: [Set Int]
digSets = do
  a <- [1 .. 6]
  b <- [a .. 7]
  c <- [b .. 8]
  d <- [c .. 9]
  [fromList [a, b, c, d] | a < b && b < c && c < d]