module Main where

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo

main = do
  print ex01
  print ex02
  print ex03
  print ex04

