module Hw3.Golf where

skip :: Int -> [a] -> [a]
skip _ [] = []
skip n (x : xs)
  | n <= 0 = []
  | otherwise = x : skip n (drop (n - 1) xs)

skips :: [a] -> [[a]]
skips l = [skip i $ drop (i - 1) l | i <- [1 .. (length l)]]

trioWise :: [c] -> [(c, c, c)]
trioWise (x : y : z : zs) = (x, y, z) : trioWise (y : z : zs)
trioWise _ = []

mid :: (a, b, c) -> b
mid (_, y, _) = y

midLargest :: (Ord a) => (a, a, a) -> Bool
midLargest (x, y, z) = y > x && y > z

localMaxima :: [Integer] -> [Integer]
localMaxima = map mid . filter midLargest . trioWise
