{-# OPTIONS_GHC -Wall #-}

module Party where

import           Data.Tree
import           Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
  (GL es1 f1) <> (GL es2 f2) = GL (es1 <> es2) (f1 + f2)

instance Monoid GuestList where
  mempty = (GL mempty 0)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = a, subForest = ns } = f a . map (treeFold f) $ ns
