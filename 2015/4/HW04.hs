{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List (intercalate)
newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

getList :: Poly a -> [a]
getList (P a) = a

dropFirstZero :: (Eq a, Num a) => [a] -> [a]
dropFirstZero [] = []
dropFirstZero (n:ns) = if n == 0 then ns else (n:ns)

instance (Num a, Eq a) => Eq (Poly a) where
    (==) p1 p2 = listA == listB
        where listA = dropFirstZero (getList p1)
              listB = dropFirstZero (getList p2)

-- Exercise 3 -----------------------------------------

showTerm :: (Eq a, Num a, Show a) => (a,Int) -> String
showTerm (0,0) = ""
showTerm (c,0) = show c
showTerm (1,1) = "x"
showTerm (-1, 1) = "-x"
showTerm (c, 1) = show c ++ "x"
showTerm (1, e) = "x^" ++ show e
showTerm (-1, e) = "-x^" ++ show e
showTerm (c,e) = show c ++ "x^" ++ show e

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show p1
      | p1 == P [0] = "0"
      | otherwise = intercalate " + " $ reverse $ map showTerm $ filter (\(c,_) -> c /= 0) $ zip (getList p1) [0..]


-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus p1 p2 = P (addLists (getList p1) (getList p2))
             where addLists :: Num b => [b] -> [b] -> [b]
                   addLists [] [] = []
                   addLists a [] = a
                   addLists [] b = b
                   addLists (a:as) (b:bs) = (a + b) : addLists as bs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times p1 p2 = sum (multPol (getList p1) (getList p2) 0)
              where multPol :: Num a => [a] -> [a] -> Int -> [Poly a]
                    multPol [] _ _ = []
                    multPol (a:as) ys n = (P ((replicate n 0) ++ (map (a *) ys))) : multPol as ys (n + 1)
                


-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate p1 = P (map (* (-1)) (getList p1))
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: (Num a) => Poly a -> a -> a
applyP poly n = sum $ map (applyTerm n) (zip (getList poly) [0..])
                where applyTerm :: (Num a) => a -> (a, Integer) -> a
                      applyTerm o (c,e) = c * (o^e)
                

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n poly 
      | n == 1 = deriv poly
      | otherwise = nderiv (n -1) (deriv poly)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv p1 = P $ go (getList p1)
      where go [] = []
            go (_:as) = zipWith (*) as (map fromInteger [1..])

