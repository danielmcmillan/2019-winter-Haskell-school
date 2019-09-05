{-# OPTIONS_GHC -Wall #-}
module Calc
  ( eval
  , evalStr
  , Expr(lit, add, mul)
  )
where

import           ExprT
import           Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x  ) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr (parseExp Lit Add Mul -> Just expr) = Just $ eval expr
evalStr _ = Nothing

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance (Expr ExprT) where
  lit = Lit
  add = Add
  mul = Mul
