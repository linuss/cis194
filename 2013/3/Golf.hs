module Golf where

-- Exercise 1 --
skip :: Integer -> [a] -> [a]
skip n list = [x | (x,y) <- zip list [1..], y `mod` n == 0]

skips :: [a] -> [[a]]
skips list = zipWith ($) (map skip [1..]) $ (replicate (length list) list)

-- Exercise 2 --

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) 
  | x < y && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

-- Exercise 3 --
histogram :: [Integer] -> String
histogram list = 

