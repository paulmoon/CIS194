{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid

-- Exercise 1
-- This should only add new Employee without doing any checks.
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL employees funScore) = GL (employees ++ [emp]) (empFun emp + funScore)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL leftEmps leftScore) (GL rightEmps rightScore) = GL (leftEmps ++ rightEmps) (leftScore + rightScore)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = case compare a b of
    LT -> b
    otherwise -> a

-- Exercise 2
-- treeFold :: -> Tree a -> b

main :: IO ()
main = do
  print $ ""


