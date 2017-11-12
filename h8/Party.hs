{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List
import Data.Ord

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
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label subtrees) = f label (map (treeFold f) subtrees)

prefixCombine :: Employee -> [Employee] -> Employee
prefixCombine boss [] = boss
prefixCombine boss subtrees = 
  foldr (\x y -> 
        Emp ((empName x) ++ " " ++ (empName y)) 
            ((empFun x) + (empFun y))) 
  boss subtrees

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (GL [boss] (empFun boss), mempty)
nextLevel boss guestLists = (newGlWithNewBoss, newGlWithoutNewBoss)
  where newGlWithNewBoss = empToGl boss <> mconcat (map snd guestLists)
        newGlWithoutNewBoss = mconcat $ map getMaxGuestList guestLists

getMaxGuestList :: (GuestList, GuestList) -> GuestList
getMaxGuestList (a, b) = case compare a b of 
  GT -> a
  otherwise -> b

empToGl :: Employee -> GuestList
empToGl emp = GL [emp] (empFun emp)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun a = getMaxGuestList (treeFold nextLevel a)

main :: IO ()
main = do
  -- print $ treeFold prefixCombine testCompany
  -- print $ testCompany
  -- print $ treeFold nextLevel testCompany
  -- print $ maxFun testCompany

  -- Exercise 5
  fileContents <- readFile "company.txt"
  let company = read fileContents :: Tree Employee
      bestGuestList = maxFun company
      getFunVal (GL _ fun) = fun
      getEmps (GL emps _) = emps

  putStrLn $ "Total fun: " ++ (show $ getFunVal bestGuestList)
  putStrLn . unlines . sort $ map empName (getEmps bestGuestList)
