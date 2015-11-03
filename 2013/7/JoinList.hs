{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

{- Exercise 1 -}
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a 
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (tag x <> tag y) x y

{- Exercise 2.1 -}
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

getNodeSize :: (Sized b, Monoid b) => JoinList b a -> Int
getNodeSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a 
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l1 l2)
  | i < sizeLeft = indexJ i l1
  | otherwise = indexJ (i - sizeLeft) l2
  where sizeLeft = getNodeSize l1 

jl = Append (Size 3) 
      (Append (Size 2) 
        (Single (Size 1) 'a')
        (Single (Size 1) 'b')
      )
      (Single (Size 1) 'c')

{- Exercise 2.2 -}

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n s@(Single _ _) 
  | n <= 0 = s
  | otherwise = Empty
dropJ n jl@(Append _ l1 l2) 
  | n <= 0 = jl
  | n > getNodeSize jl = Empty
  | n < getNodeSize l1 = dropJ n l1 +++ l2
  | n >= getNodeSize l1 = dropJ (n - getNodeSize l1) l2
  | otherwise = jl

{- Exercise 2.3 -}

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n s@(Single _ _ )
  | n <= 0 = Empty
  | otherwise = s 
takeJ n jl@(Append _ l1 l2)
  | n <= 0 || n > getNodeSize jl = jl
  | n > getNodeSize l1 = l1 +++ (takeJ (n - getNodeSize l1) l2)
  | otherwise = takeJ n l1
  
{- Exercise 3 -} 

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine s = Single (scoreString s) s


{- Exercise 4 -}

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList

  fromString "" = Empty
  fromString [x] = Single (score x, 1) [x]
  fromString x = foldr (+++) Empty . fmap fromString . lines $ x

  line = indexJ

  replaceLine n s b = takeJ (n-1) b +++ fromString s +++ dropJ n b

  numLines = getSize . snd . tag 

  value = getScore . fst . tag

intro :: JoinList (Score, Size) String
intro = fromString "This buffer is for notes you don't want to save, and for" +++
        fromString "evaluation of steam valve coefficients." +++
        fromString "To load a different file, type the character L followed" +++
        fromString "by the name of the file."

main = runEditor editor intro


