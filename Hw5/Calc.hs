{-# LANGUAGE FlexibleInstances #-}

module Hw5.Calc where

import Hw5.ExprT
import Hw5.Parser
import qualified Hw5.StackVM as VM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr = parseExp lit add mul

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 lhs) (Mod7 rhs) = lit (lhs + rhs)
  mul (Mod7 lhs) (Mod7 rhs) = lit (lhs * rhs)

instance Expr VM.Program where
  lit n = [VM.PushI n]
  add lhs rhs = lhs ++ rhs ++ [VM.Add]
  mul lhs rhs = lhs ++ rhs ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
