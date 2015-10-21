{- Exercise 1 -}
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even 

fun2 :: Integer -> Integer
fun2 = sum . filter even . (takeWhile (/=1) . iterate collatz)
  where collatz :: Integer -> Integer 
        collatz n = if even n then n `div` 2 else 3 * n + 1

{- Exercise 2 -}
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getDepth :: Tree a -> Integer
getDepth Leaf = 0 
getDepth (Node depth _ _ _ ) = depth

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node depth left v right) 
  | getDepth left < getDepth right = Node newDepth (insert x left) v right
  | otherwise = Node newDepth left v (insert x right)
  where newDepth = (max (getDepth left) (getDepth right)) + 1

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf 

{- Exercise 3 -}
xor :: [Bool] -> Bool
xor = foldr (/=) False 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\y ys -> (f y) : ys) [] 

