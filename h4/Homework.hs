module Main where

import Data.List
import Data.Tree hiding (Tree, Node)

-- gt100 :: Int -> Bool
-- gt100 x = x >= 100

-- greaterThan100 :: [Int] -> [Int]
-- greaterThan100 xs = filter gt100 xs

-- greaterThan100v2 :: [Int] -> [Int]
-- greaterThan100v2 xs = filter (\x -> x > 100) xs

-- greaterThan100v3 :: [Int] -> [Int]
-- greaterThan100v3 xs = filter (> 100) xs

-- foobar :: [Integer] -> Integer
-- foobar = sum . map (\x -> 7*x + 2) . filter (>3)

-- Wholemeal Programming
-- 1.1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1_better :: [Integer] -> Integer
fun1_better = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
 | even n = n + fun2 ( n `div` 2)
 | otherwise = fun2 (3 * n + 1)  

fun2_better :: Integer -> Integer
fun2_better = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then 
  x `div` 2 else 3 * x + 1)

-- 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Generates a balanced binary tree from a list of values using foldr.
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree x = Node 1 (foldTree left) middle (foldTree right)
  where (left, middle, right) = halve x

createNode :: Tree a -> a -> Tree a -> Integer -> Tree a
createNode left middle right height = Node height left middle right

halve :: [a] -> ([a], a, [a])
halve x = (take half x, x!!half, drop (half + 1) x)
  where half = length x `div` 2

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree x Leaf = Node 0 Leaf x Leaf
insertIntoTree x tree@(Node height left middle right)
  -- | leftBalanced && not rightBalanced = Node height left middle (insertIntoTree x right)
  -- | not leftBalanced && rightBalanced = insertIntoTree x left
  -- | otherwise = 
    -- if leftHeight <= rightHeight 
    --   then (Node heightinsertIntoTree x left
    --   else insertIntoTree x right
    | leftHeight < rightHeight = Node height (insertIntoTree x left) middle right
    | leftHeight > rightHeight = Node height left middle (insertIntoTree x right)
    | isFull tree = Node (height+1) (insertIntoTree x left) middle right
    | leftFull && not rightFull = Node height left middle (insertIntoTree x right)
    | not leftFull && rightFull = Node height (insertIntoTree x left) middle right
    | otherwise = Node height (insertIntoTree x left) middle right
    where leftFull = isFull left
          rightFull = isFull right
          leftHeight = getHeight left
          rightHeight = getHeight right

foldTree2 :: [a] -> Tree a
foldTree2 = foldr insertIntoTree Leaf

getHeight :: Tree a -> Int
getHeight Leaf = 0
getHeight (Node n left middle right) = 1 + max (getHeight left) (getHeight right)

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node height left middle right) = 
  isBalanced left && 
  isBalanced right && 
  (getHeight(left) - getHeight(right) <= 1)

isFull :: Tree a -> Bool
isFull Leaf = False
isFull (Node _ Leaf _ Leaf) = True
isFull (Node _ left _ right) = isFull(left) && isFull(right)

showTree :: Show a => Tree a -> Int -> String
showTree Leaf _ = []
showTree (Node height l middle r) n = replicate n '*' ++ show middle ++ "\n" ++ showTree l (n+1) ++ showTree r (n+1)

-- 3: More folds!
xor :: [Bool] -> Bool
xor = foldr doXor False where
  doXor newBool existingBool = if newBool then not existingBool else existingBool

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr doMap [] where
  doMap newElem existingElems = f newElem : existingElems

-- fold using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldl f base xs

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

-- 4: Finding primes

-- Given an integer n, generate all the odd prime numbers up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filter (\x -> notElem x numbersToFilter) [1..n]
  where numbersToFilter = map (\(i, j) -> i + j + 2*i*j) . takeWhile (\(i, j) -> i <= j && i + j + 2*i*j <= n) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

main = do
  -- print $ greaterThan100v2 [1, 300, 26, 1, 99, 100]
  -- print $ greaterThan100v3 [1, 300, 26, 1, 99, 100]
  -- print $ (\x y z -> [x, 2*y, 3*z]) 5 7 9
  -- print $ foobar [1, 3, 5]
  -- print $ "Hello world"
  -- print $ fun1 [1, 3, 5] == fun1_better [1, 3, 5]
  -- print $ fun1 [2, 3, 9] == fun1_better [2, 3, 9]
  -- print $ fun1 [] == fun1_better []

  -- print $ fun2 1 == fun2_better 1
  -- print $ fun2 2 == fun2_better 2
  -- print $ insertIntoTree (insertIntoTree (insertIntoTree (insertIntoTree Leaf "A") "B") "C") "D"
  -- print $ foldTree "ABCD"
  -- print $ foldTree2 "ABCDEFGHI"
  -- putStrLn $ showTree (foldTree2 "ABCDEFGHIJKLMNOPQRSTUVWXYZ") 0

  print $ xor [True, False, False]
  print $ xor [True, False, True]
  print $ map2 (+1) [1, 2, 3, 4]
  print $ map2 (*10) [1, 2, 3, 4]
  print $ myFoldl (*) 1 [2, 3, 4, 5] -- 5! = 120
  print $ myFoldl (+) 1 [2, 3, 4, 5] -- 5 * 6 / 2 = 15

  print $ sieveSundaram 50
