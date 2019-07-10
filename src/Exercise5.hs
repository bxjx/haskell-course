{-# LANGUAGE FlexibleInstances #-}
module Exercise5 where

import ExprT
import Parser
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just expr -> Just (eval expr)
  Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i = i > 0
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

class HasVars a where
  vars :: String -> a

-- did not continue on this.
