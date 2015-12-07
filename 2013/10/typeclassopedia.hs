module Typeclassopedia where

data Either' a b = Left' a | Right' b
  deriving (Eq, Ord, Read, Show)

instance Functor (Either' e) where
  fmap _ (Left' a) = Left' a
  fmap f (Right' a) = Right' (f a)

data Pair a = Pair a a 
  deriving (Show, Eq, Ord, Read)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf g) = Leaf (f . g)
  fmap f (Node trees) = Node (fmap (fmap f) trees)

