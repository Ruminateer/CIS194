{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Hw5.Calc where

import qualified Data.Map as M
import qualified Hw5.ExprT as ET
import Hw5.Parser
import qualified Hw5.StackVM as VM

eval :: ET.ExprT -> Integer
eval (ET.Lit n) = n
eval (ET.Add e1 e2) = eval e1 + eval e2
eval (ET.Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr = parseExp lit add mul

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ET.ExprT where
  lit = ET.Lit
  add = ET.Add
  mul = ET.Mul

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

class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftA2 (liftA2 (+))
  mul = liftA2 (liftA2 (*))

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
