module Hw3.Golf where

import qualified Data.Vector.Unboxed as VU

-- Exercise 1 Hopscotch
skip :: Int -> [a] -> [a]
skip _ [] = []
skip n (x : xs)
  | n <= 0 = []
  | otherwise = x : skip n (drop (n - 1) xs)

skips :: [a] -> [[a]]
skips l = [skip i $ drop (i - 1) l | i <- [1 .. length l]]

-- Exercise 2 Local maxima
trioWise :: [c] -> [(c, c, c)]
trioWise (x : y : z : zs) = (x, y, z) : trioWise (y : z : zs)
trioWise _ = []

mid :: (a, b, c) -> b
mid (_, y, _) = y

midLargest :: (Ord a) => (a, a, a) -> Bool
midLargest (x, y, z) = y > x && y > z

localMaxima :: [Integer] -> [Integer]
localMaxima = map mid . filter midLargest . trioWise

-- Exercise 3 Histogram

histogram :: [Integer] -> String
histogram nums = go (countNumbers nums) "==========\n0123456789\n"
  where
    countNumbers :: [Integer] -> [Int]
    countNumbers = VU.toList . VU.accum (+) (VU.replicate 10 0) . map (\n -> (fromInteger n, 1))

    go :: [Int] -> String -> String
    go cnts acc
      | all (== 0) cnts = acc
      | otherwise = let (nextCnts, currentLine) = histogramLine cnts in go nextCnts (currentLine ++ '\n' : acc)

    histogramLine :: [Int] -> ([Int], String)
    histogramLine = unzip . map (\n -> if n > 0 then (n - 1, '*') else (n, ' '))
