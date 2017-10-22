-- CIS194 Homework #6: Laziness

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib (-1) = (-1)
fib n | n >= 2 = fib (n - 1) + fib (n - 2)
fib n | n <= -2 = fib (n + 1) + fib (n + 2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2!!(n-1) + fibs2!!(n-2) | n <- [2..]]

-- Exercise 3
data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show stream = show $ take 50 $ streamToList stream

streamToList :: Stream a -> [a]
streamToList (Stream a b) = [a] ++ streamToList b

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat n = Stream n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a rest) = Stream (f a) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream seed (streamFromSeed f (f seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

nthStream :: Integer -> Stream Integer
nthStream n = interleaveStreams (streamRepeat n) (nthStream (n+1))

ruler :: Stream Integer
ruler = nthStream 0

-- Alternates elements from first and second streams.
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a aRest) rest = Stream a (interleaveStreams rest aRest)

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger i = Stream i (streamRepeat 0)
  (+) (Stream a aRest) (Stream b bRest) = (Stream (a + b) (aRest + bRest))
  (*) (Stream a aRest) bStream@(Stream b bRest) = 
    Stream (a * b) $ (streamMap (*a) bRest) + (aRest * bStream)
  negate (Stream a aRest) = Stream (-a) (negate aRest)

instance Fractional (Stream Integer) where
  (/) aStream@(Stream a aRest) bStream@(Stream b bRest) = 
    Stream (div a b) $ streamMap (* (div 1 b)) (aRest - (aStream * bRest / bStream))

-- MAGIC
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Fibonacci via matrices
-- Proof by induction

-- F(n+1) + F(n)     | F(n+1)
-- F(n) + F(n-1)     | F(n)

-- Let m = n+2. m - 2 = n

-- F(m-1) + F(m-2) = F(m)       | F(m-1)
-- F(m-2) + F(m-3) = F(m-1)     | F(m-2)

-- Matrix a b c d = [ a b ]
--                  [ c d ]
data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show)

-- [ a1 b1 ] * [ a2 b2 ] = [ a1*a2 + b1*c2 , a1*b2 + b1*d2 ]
-- [ c1 d1 ]   [ c2 d2 ]   [ c1*a2 + d1*c2 , c1*b2 + d1*d2 ]
instance Num (Matrix) where
  (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) = 
    (Matrix (a1 * a2 + b1 * c2) (a1 * b2 + b1 * d2) (c1 * a2 + d1 * c2) (c1 * b2 + d1 * d2))

fibs4 :: Integer -> Integer 
fibs4 n = a
  where (Matrix a b c d) = fibs4_helper n 

-- Raise F^n where F = Matrix 1 1 1 0
fibs4_helper :: Integer -> Matrix
fibs4_helper 0 = Matrix 1 1 1 0
fibs4_helper 1 = Matrix 1 1 1 0
fibs4_helper n = case even n of
  True -> (fibs4_helper (div n 2)) * fibs4_helper (div n 2)
  otherwise -> matrix * ((fibs4_helper (div (n-1) 2)) * fibs4_helper (div (n-1) 2))
  where matrix = Matrix 1 1 1 0

main :: IO()
main = do 
  print $ [fib x | x <- [0.. 10]]
  print $ [fib x | x <- [0, -1.. - 10]]
  print $ take 10 $ fibs2
  print $ show nats
  print $ show $ interleaveStreams nats nats
  print $ show $ ruler
  print $ (x^4)
  print $ (1 + x)^5
  print $ (x^2 + x + 3) * (x - 5)
  print $ fibs3
  print $ [fibs4 x | x <- [0.. 100]]
  print $ fibs4 100000
