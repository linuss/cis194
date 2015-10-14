-- Exercise 1 --

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev n 
  | n > 9 = (n `mod` 10) : toDigitsRev (n `div` 10)
  | otherwise = [n]

toDigits n = reverse $ toDigitsRev n

-- Exercise 2 --

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther list = reverse $ go $ reverse list
  where go :: [Integer] -> [Integer]
        go [] = []
        go [x] = [x]
        go (x:y:xs) =  x:(y*2): go xs

-- Exercise 3 --
sumDigits :: [Integer] -> Integer
sumDigits list = sum $ concatMap toDigits list

-- Exercise 4 --
validate :: Integer -> Bool
validate cc = mod (sumDigits $ doubleEveryOther $ toDigits cc) 10 == 0
