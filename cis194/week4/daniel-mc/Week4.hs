{-# OPTIONS_GHC -Wall #-}
module Week4
  ( fun1
  , fun2
  , insert
  , Tree(Leaf, Node)
  , foldTree
  )
where

-- Exercise 1

fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x : xs) | even x    = (x - 2) * fun1 xs
--               | otherwise = fun1 xs
-- filters to even, subtracts 2, gets product
fun1 = product . map (subtract 2) . filter even

fun2Sequence :: Integer -> Integer
fun2Sequence n | even n    = n `div` 2
               | otherwise = 3 * n + 1

fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n    = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)
-- sums even numbers from a sequence ending at 1
fun2 = sum . filter even . takeWhile (> 1) . iterate fun2Sequence

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight (Node h _ _ _) = h
treeHeight Leaf           = -1

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l v r)
  | treeHeight l <= treeHeight r
  = let l' = insert x l in Node (max (treeHeight l') (treeHeight r) + 1) l' v r
  | otherwise
  = let r' = insert x r in Node (max (treeHeight l) (treeHeight r') + 1) l v r'

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
