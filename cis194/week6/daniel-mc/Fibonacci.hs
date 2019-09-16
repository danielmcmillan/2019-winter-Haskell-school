{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Fibonacci
  ( fib
  , fibs1
  , fibs2
  , streamToList
  , streamRepeat
  , streamMap
  , streamFromSeed
  , nats
  , ruler
  )
where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]


-- Exercise 2
iterate2 :: (a -> a -> a) -> a -> a -> [a]
iterate2 f x y = x : y : iterate2 f x' (f y x') where x' = f x y

fibs2 :: [Integer]
fibs2 = iterate2 (+) 0 1

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- Alternates items from two streams
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) y = Stream x $ interleaveStreams y xs

-- Recursively interleave streams so that each number occurs half as often as the last
-- Requires interleaveStreams to be lazy in the second argument
rulerGen :: Integer -> Stream Integer
rulerGen x = interleaveStreams (streamRepeat x) (rulerGen $ x + 1)

ruler :: Stream Integer
ruler = rulerGen 0
