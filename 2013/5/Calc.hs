{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import ExprT
import Parser 
import qualified StackVM as VM
import qualified Data.Map as M

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
class Expr a where 
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a 

instance Expr ExprT where
  lit a = Lit a
  add a b = Add a b
  mul a b = Mul a b


reify :: ExprT -> ExprT
reify = id

{- Exercise 4 -}
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a 
    | a <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

{- Exercise 5 -}
instance Expr VM.Program where
  lit i = [VM.PushI i]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile s = parseExp lit add mul s :: Maybe VM.Program

run :: String -> Maybe (Either String VM.StackVal)
run s = do
  program <- compile s
  return $ VM.stackVM program

{- Exercise 6 -}

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String 
  deriving (Show, Eq)

instance Expr VarExprT where
  lit a = VLit a
  add a b = VAdd a b
  mul a b = VMul a b

instance HasVars VarExprT where
  var = Var


