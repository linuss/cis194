{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do
  x <- mx
  return $ f x

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f mx = mx >>= \x -> return (f x)
  

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV x y v = do
  mx <- v !? x
  my <- v !? y
  return $ v // [(y,mx), (x,my)]


-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM mf (x:xs) = do
  mx <- mf x
  result <- mapM mf xs
  return $ (mx : result)
  

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices vector = mapM (vector !?) indices


-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vector
  | length vector == 0 = return Nothing 
  | otherwise = do
      i <- getRandomR (0, (length vector))
      return $ vector !? i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (minRange, maxRange) = V.replicateM n (getRandomR (minRange,maxRange))

-- Exercise 5 -----------------------------------------

swap :: (Int, Int) -> Vector a -> Vector a
swap (i,j) vector = vector // [(i, (vector ! j)), (j, (vector ! i))]


shuffle :: Vector a -> Rnd (Vector a)
shuffle vector = do
  let from = V.reverse $ V.enumFromN 1 (n-1) 
  to <- V.mapM getRandomR (V.zip (V.replicate n 0) from)
  return $  V.foldr' swap vector (V.zip from to)
  where n = length vector

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vector n 
  | n > length vector || n < 0 = error "N is out of bounds"
  | otherwise = (V.filter (\x -> x < y) vector, y, V.filter (\x -> x > y) vector)
                where y = vector ! n
  

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vector 
 | null vector = V.empty
 | otherwise = qsort [ y | y <- V.tail vector, y < V.head vector]
                <> (cons (V.head vector) (qsort [ y | y <- V.tail vector, y >= V.head vector]))

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vector  
  | null vector = return V.empty
  | otherwise = do
  pivotIndex <- getRandomR (0, (length vector) - 1)
  let split = partitionAt vector pivotIndex
  left <- qsortR $ first split
  right <- qsortR $ third split
  return $ left <> (cons (second split)right)
  where first :: (Vector a, a, Vector a) -> Vector a
        first (x,_,_) = x
        second :: (Vector a, a, Vector a) -> a
        second (_,x,_) = x
        third :: (Vector a, a, Vector a) -> Vector a
        third (_,_,x) = x

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
