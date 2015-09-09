{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches secret guess  = go 0 secret guess
  where go :: Int -> Code -> Code -> Int
        go acc _ [] = acc
        go acc [] _ = acc
        go acc (x:xs) (y:ys) 
          | x == y = go (acc + 1) xs ys 
          | otherwise = go acc xs ys

-- Exercise 2 -----------------------------------------

-- Count how many times a peg/color is in a list
countColor :: Code -> Peg -> Int
countColor code color = go 0 code color
  where go :: Int -> Code -> Peg -> Int
        go acc [] _ = acc
        go acc (x:xs) c 
          | x == c = go (acc + 1) xs c
          | otherwise = go acc xs c

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (countColor code) colors 

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess = sum $ zipWith min (countColors secret) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess ems ((matches guess secret) - ems)
                        where ems = exactMatches guess secret

-- Exercise 4 -----------------------------------------

getCode :: Move -> Code
getCode (Move code _ _) = code

getExactMatches :: Move -> Int
getExactMatches (Move _ x _) = x

getRegularMatches :: Move -> Int
getRegularMatches (Move _ _  x) = x

isConsistent :: Move -> Code -> Bool
isConsistent move secret = (getExactMatches move == getExactMatches newMove && 
                            getRegularMatches move == getRegularMatches newMove)
                            where newMove = (getMove secret (getCode move)) 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

addColor :: [Code] -> Peg -> [Code]
addColor codes color = map (color :) codes

allCodes :: Int -> [Code]
allCodes len 
  | len <= 0 = []
  | len == 1 = map (\x -> [x]) colors 
  | otherwise = concatMap (addColor (allCodes (len - 1))) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = go secret (allCodes (length secret)) []
  where go :: Code -> [Code] -> [Move] -> [Move]
        go secret (x:xs) moves
          | secret == x = moves ++ [currentMove]
          | otherwise = go secret (filterCodes currentMove xs) (moves ++ [currentMove])
          where currentMove = getMove secret x
        go _ _ _ = error "shouldn't happen!"




-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
