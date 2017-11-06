module Playground where

import Data.Monoid

data Tree a = Empty
            | Node (Tree a) a (Tree a)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> x + l + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

newtype Sum a = Sum a
    deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = (+)

newtype Product a = Product a
    deriving (Eq, Ord, Num, Show)

getSum :: Product a -> a
getSum (Product a) = a

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend = (*)

lst :: [Integer]
lst = [1,5,8,23,423,99] 

main :: IO()
main = do
  putStrLn "Hello World"
  print $ treeSize' (Node Empty 1 Empty)
  print $ treeSum' (Node (Node Empty 2 Empty) 1 Empty)
  print $ treeDepth' (Node (Node Empty 2 Empty) 1 Empty)
  print $ flatten' (Node (Node Empty 2 Empty) 1 Empty)
  
