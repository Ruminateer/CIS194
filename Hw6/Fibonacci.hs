{-# LANGUAGE FlexibleInstances #-}

module Hw6.Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0 ..]]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

-- Exercise 3

data Stream a = a :> Stream a

infixr 5 :>

instance (Show a) => Show (Stream a) where
  show stream = '[' : go stream (20 :: Int)
    where
      go _ 0 = "...]"
      go (e :> rest) n = show e ++ ',' : go rest (n - 1)

streamToList :: Stream a -> [a]
streamToList (e :> rest) = e : streamToList rest

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat e = e :> streamRepeat e

instance Functor Stream where
  fmap fn (e :> rest) = fn e :> fmap fn rest

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn seed = seed :> streamFromSeed fn (fn seed)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (a :> as) (b :> bs) = a :> b :> interleaveStreams as bs

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (fmap (+ 1) ruler)

-- Exercise 6

x :: Stream Integer
x = 0 :> 1 :> streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = n :> streamRepeat 0
  negate = fmap (* (-1))
  (a :> as) + (b :> bs) = (a + b) :> (as + bs)
  (a :> as) * (b :> bs) = a * b :> fmap (* a) bs + fmap (* b) as + as * bs

instance Fractional (Stream Integer) where
  (a :> as) / (b :> bs) = q
    where
      q = (a `div` b) :> fmap (`div` b) (as - q * bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)
