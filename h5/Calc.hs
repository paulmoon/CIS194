{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Data.Maybe
import Parser
import qualified Data.Map as M
import qualified ExprT as E
import qualified StackVM

-- Exercise 1
eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add left right) = (eval left) + (eval right)
eval (E.Mul left right) = (eval left) * (eval right)

-- Exercise 2: Ready for "synergizing with the UI department"
evalStr :: String -> Maybe Integer
evalStr s = case parseExp E.Lit E.Add E.Mul s of
  Just n -> Just (eval n)
  Nothing -> Nothing

-- Exercise 3: 
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit n = E.Lit n
  add left right = E.Add left right
  mul left right = E.Mul left right

-- Exercise 4: 
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit n = n
  add a b = a + b
  mul a b = a * b 

instance Expr Bool where
  lit n = n > 0
  add a b = a || b
  mul a b = a && b 

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax a) (MinMax b) = if a > b then (MinMax a) else (MinMax b)
  mul (MinMax a) (MinMax b) = if a > b then (MinMax b) else (MinMax a)

instance Expr Mod7 where
  lit n = Mod7 n
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5: Custom CPU
instance Expr StackVM.Program where
  lit n = [StackVM.PushI n]
  add left right = left ++ right ++ [StackVM.Add]
  mul left right = left ++ right ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = parseExp lit add mul s

executeVm :: String -> Either String StackVM.StackVal
executeVm = StackVM.stackVM . fromMaybe [] . compile

-- Exercise 6: Intermediate values
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
           | Var String
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit n = Lit n
  add a b = Add a b
  mul a b = Mul a b

instance HasVars VarExprT where
  var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)
  add f g = \x -> case isNothing (f x) of
      True -> if isNothing (g x) then Nothing else Just (fromJust (g x))
      False -> if isNothing (g x) then Just (fromJust (f x)) else Just (fromJust (f x) + fromJust (g x))
  mul f g = \x -> case isNothing (f x) || isNothing (g x) of
      True -> Nothing
      _ -> Just (fromJust (f x) * fromJust (g x))

withVars :: [(String, Integer)] 
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

main :: IO()
main = do
  print $ eval (E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4)) == 20

  print $ evalStr "(2+3)*4" == Just 20
  print $ evalStr "2+3*4" == Just 14
  print $ evalStr "2+3*" == Nothing

  print $ (mul (add (lit 2) (lit 3)) (lit 4) :: E.ExprT) 
          == (E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4))

  print $ testInteger
  print $ testBool
  print $ testMM
  print $ testSat

  print $ executeVm "2 + 3 * 4"

  print $ (add (lit 3) (var "x") :: VarExprT)
  print $ withVars [("x", 6)] $ add (lit 3) (var "x")
  print $ withVars [("x", 6)] $ add (lit 3) (var "y")
  print $ withVars [("x", 6), ("y", 3)]
          $ (mul (var "x") (add (var "y") (var "x")))

  print $ withVars [("x", 6), ("y", 3)] 
          $ (mul (var "z") (var "x")) -- Should be Nothing

  print $ withVars [("x", 6), ("y", 3)] 
          $ (mul (var "x") (var "y")) -- 6 * 3 = 18
