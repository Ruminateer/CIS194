module Hw1.Hanoi where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst tmp
  | n <= 0 = []
  | otherwise = hanoi (n - 1) src tmp dst ++ [(src, dst)] ++ hanoi (n - 1) tmp dst src

hanoiAcc :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiAcc n source dest temp = go n source dest temp []
  where
    go k src dst tmp rest
      | k <= 0 = rest
      | otherwise = go (k - 1) src tmp dst ((src, dst) : go (k - 1) tmp dst src rest)
