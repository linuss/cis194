module Golf where

import Data.List (transpose, intercalate)

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

line :: (Eq a) => [a] -> (a -> a -> Bool) -> a -> String 
line list comp n = replicate x '*' ++ replicate (9 - x) ' '
  where x = length $ filter (\x -> x `comp` n) list

nonEmptyString :: String -> Bool
nonEmptyString str = length ((filter (\x -> x /= ' ')) str) > 0 

histogram :: [Integer] -> String
histogram list = (intercalate "\n" $ reverse $ filter nonEmptyString $ transpose $ map (line list (==)) [0..9]) ++ "\n==========\n0123456789\n"

