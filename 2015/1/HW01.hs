{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n
  | n < 10 = n
  | otherwise = lastDigit (n `mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n
 | n < 10 = 0
 | otherwise = n `div` 10

-- Exercise 2 -----------------------------------------

-- Split a positive integer into a list of digits, reversed
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = (lastDigit n) : (toRevDigits (dropLastDigit n))

-- Split a positive integer into a list of digits
toDigits :: Integer -> [Integer]
toDigits n = reverse (toRevDigits n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : (2 * y) : (doubleEveryOther zs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] 
  | x < 10 = x
  | otherwise = lastDigit x + (sumDigits [(dropLastDigit x)])
sumDigits (x:xs) = (sumDigits [x]) + (sumDigits xs)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (sumDigits (doubleEveryOther (toRevDigits n))) `mod` 10 == 0 
  

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
