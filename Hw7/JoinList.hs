module Hw7.JoinList where

import Hw7.Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single annotation _) = annotation
tag (Append annotation _ _) = annotation

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
lhs +++ rhs = Append (tag lhs <> tag rhs) lhs rhs

-- Exercise 2

lengthJ :: (Sized b, Monoid b) => JoinList b a -> Int
lengthJ = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl@(Append _ lhs rhs)
  | i < 0 = Nothing
  | i >= lengthJ jl = Nothing
  | i < lengthJ lhs = indexJ i lhs
  | otherwise = indexJ (i - lengthJ lhs) rhs
indexJ _ Empty = Nothing
indexJ i (Single _ v)
  | i == 0 = Just v
  | otherwise = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl@(Append _ lhs rhs)
  | n <= 0 = jl
  | n >= lengthJ jl = Empty
  | otherwise = (dropJ n lhs) +++ (dropJ (n - lengthJ lhs) rhs)
dropJ _ Empty = Empty
dropJ n jl@(Single _ _)
  | n > 0 = Empty
  | otherwise = jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl@(Append _ lhs rhs)
  | n <= 0 = Empty
  | n >= lengthJ jl = jl
  | otherwise = (takeJ n lhs) +++ (takeJ (n - lengthJ lhs) rhs)
takeJ _ Empty = Empty
takeJ n jl@(Single _ _)
  | n > 0 = jl
  | otherwise = Empty
