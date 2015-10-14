{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a b) = Cons (f a) $ fmap f b

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons h t) s = Cons h $ sInterleave s t

sTake :: Int -> Stream a -> [a]
sTake n (Cons h t) 
  | n < 1 = []
  | n == 1 = [h]
  | otherwise = h : sTake (n - 1) t

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = go 0 
  where go :: Integer -> Stream Integer
        go n = sInterleave (sRepeat n) (go (n+1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons seed $ rand ((1103515245 * seed + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 227 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax list = Just $ go (minBound :: Int) (maxBound :: Int) list
  where go :: Int -> Int -> [Int] -> (Int, Int)
        go max min [] = (min, max)
        go max min (x:xs)
          | x < min = go max x xs 
          | x > max = go x min xs
          | otherwise = go max min xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
