{-# OPTIONS_GHC -Wall #-}
module Week4
  ( fun1
  , fun2
  )
where

fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x : xs) | even x    = (x - 2) * fun1 xs
--               | otherwise = fun1 xs
-- filters to even, subtracts 2, gets product
fun1 = product . map (subtract 2) . filter even

fun2Sequence :: Integer -> Integer
fun2Sequence n | even n = n `div` 2
  | otherwise = 3 * n + 1

fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n    = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)
-- sums even numbers from a sequence ending at 1
fun2 = sum . filter even . takeWhile (>1) . iterate fun2Sequence
