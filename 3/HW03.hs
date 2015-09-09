module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name val = \x -> if x == name then val else state x
  
  

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalBop :: Bop -> Int -> Int -> Int 
evalBop Plus x y = x + y
evalBop Minus x y = x - y
evalBop Times x y = x * y
evalBop Divide x y = x `div` y
evalBop Gt x y = if x > y then 1 else 0
evalBop Ge x y = if x >= y then 1 else 0
evalBop Lt x y = if x < y then 1 else 0
evalBop Le x y = if x <= y then 1 else 0
evalBop Eql x y = if x == y then 1 else 0


evalE :: State -> Expression -> Int
evalE state (Var name) = state name
evalE state (Val x) = x
evalE state (Op exp1 bop exp2) = evalBop bop (evalE state exp1) (evalE state exp2)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign str exp) = DAssign str exp
desugar (If exp stm1 stm2) = DIf exp (desugar stm1) (desugar stm2)
desugar (While exp stm) = DWhile exp (desugar stm)
desugar (Sequence stm1 stm2) = DSequence (desugar stm1) (desugar stm2)
desugar Skip = DSkip
desugar (Incr x) = DAssign x (Op (Var x) Plus (Val 1))
desugar (For stm1 exp stm2 stm3) = DSequence (desugar stm1) (DWhile exp (DSequence (desugar stm3)  (desugar stm2))) 


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign str exp) = (extend state str (evalE state exp))
evalSimple state (DIf exp stm1 stm2)
  | evalE state exp == 0 = evalSimple state stm2
  | otherwise = evalSimple state stm1
evalSimple state (DWhile exp stm) 
  | evalE state exp == 1 = evalSimple (evalSimple state stm) (DWhile exp stm)
  | otherwise = state
evalSimple state (DSequence stm1 stm2) = evalSimple (evalSimple state stm1) stm2 
evalSimple state (DSkip) = state

run :: State -> Statement -> State
run state stm = evalSimple state (desugar stm)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
