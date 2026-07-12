module Hw1.Hanoi where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoiAcc :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiAcc n a b c = go n a b c []
  where
    go k from to temp rest
      | k <= 0 = rest
      | otherwise =
          go
            (k - 1)
            from
            temp
            to
            ((from, to) : go (k - 1) temp to from rest)
