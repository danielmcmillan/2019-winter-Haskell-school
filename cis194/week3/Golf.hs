{-# OPTIONS_GHC -Wall #-}

-- Exercise 1

-- Filter list to every nth element, starting from the first
everyNth :: Int -> [a] -> [a]
everyNth n (x : xs) = x : everyNth n (drop (n - 1) xs)
everyNth _ _        = []

skipsN :: Int -> [a] -> [[a]]
skipsN n x@(_ : ys) = everyNth n x : skipsN (n + 1) ys
skipsN _ _          = []

skips :: [a] -> [[a]]
skips = skipsN 1

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x : xs@(y : z : _)) | y > x && y > z = y : localMaxima xs
                                 | otherwise      = localMaxima xs
localMaxima _ = []

-- Exercise 3

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

counts :: [Integer] -> [Int]
counts x = map (`count` x) [0 .. 9]

histogramChar :: Bool -> Char
histogramChar True  = '*'
histogramChar False = ' '

histogramLine :: [Int] -> Int -> String
histogramLine cs x = map (histogramChar . (>= x)) cs

histogramLines :: [Int] -> Int -> [String]
histogramLines cs x@(histogramLine cs -> l) | l /= replicate 10 ' ' = l : histogramLines cs (x + 1)
                                            | otherwise             = []

histogram :: [Integer] -> String
histogram ds = unlines . reverse $ (['0' .. '9'] : replicate 10 '=' : histogramLines (counts ds) 1)
