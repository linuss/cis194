module Party where

import Employee
import Data.Monoid
import Data.Tree

{- Exercise 1 -}
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL [] _) = GL [emp] (empFun emp)
glCons emp (GL list fun) = GL (emp : list) (fun + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL list1 fun1) (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

{- Exercise 2 -}
  
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a forest) = f a (map (treeFold f) forest)

{- Exercise 3 -}

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, mempty)
nextLevel boss guestlists = (glCons boss bestListWithoutBoss,  bestListWithBoss)
  where bestListWithBoss = maximum $ map fst guestlists
        bestListWithoutBoss = maximum $ map snd guestlists

{- Exercise 4 -}
maxFun :: Tree Employee -> GuestList
maxFun employees = uncurry moreFun $ treeFold nextLevel employees

{- Exercise 5 -}

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

getEmps :: GuestList -> [Employee]
getEmps (GL emps _) = emps

main :: IO ()
main = do
  file <- readFile "company.txt"
  let result = maxFun $ read file 
  putStrLn $ "Total fun: "  ++ (show $ getFun result)
  mapM_ putStrLn (map empName (getEmps result))
  

