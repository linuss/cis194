module Calc where

import ExprT
import Parser 

{- Exercise 1 -}
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

{- Exercise 2 -}
evalStr :: String -> Maybe Integer
evalStr = go . parseExp Lit Add Mul 
  where go :: Maybe ExprT -> Maybe Integer
        go Nothing = Nothing
        go (Just x) = Just (eval x)

{- Exercise 3 -}

