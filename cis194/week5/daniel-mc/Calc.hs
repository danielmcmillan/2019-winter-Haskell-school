{-# OPTIONS_GHC -Wall #-}
module Calc
  ( eval
  , evalStr
  , Expr(lit, add, mul)
  , MinMax(MinMax)
  , Mod7(Mod7)
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

-- Exercise 4
instance (Expr Integer) where
  lit = id
  add = (+)
  mul = (*)

instance (Expr Bool) where
  lit x | x > 0     = True
        | otherwise = False
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
instance (Expr MinMax) where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7    = Mod7 Integer deriving (Eq, Show)
instance (Expr Mod7) where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
