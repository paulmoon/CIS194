{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: (RandomGen g) => Rand g Int
die = getRandomR (1, 6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die)

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- Exercise 2

calculateMaxAttackers :: Int -> Int
calculateMaxAttackers n 
  | n <= 1 = 0
  | otherwise = min (n-1) 3

calculateMaxDefenders :: Int -> Int
calculateMaxDefenders n 
  | n <= 0 = 0
  | otherwise = min n 2

sortRolls :: Rand StdGen [Int] -> Rand StdGen [Int]
sortRolls = liftM $ sortBy (flip compare)

getAttackerCasualties :: (Int, Int) -> Int -> Int
getAttackerCasualties (attackerRoll, defenderRoll) x = if attackerRoll > defenderRoll then x else x + 1

getDefenderCasualties :: (Int, Int) -> Int -> Int
getDefenderCasualties (attackerRoll, defenderRoll) x = if attackerRoll <= defenderRoll then x else x + 1

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield currAttackers currDefenders) = do 
  attackingDiceRolls <- sortRolls $ dice (calculateMaxAttackers currAttackers)
  defendingDiceRolls <- sortRolls $ dice (calculateMaxDefenders currDefenders)

  let x = foldr getAttackerCasualties 0 (zip attackingDiceRolls defendingDiceRolls)
  let y = foldr getDefenderCasualties 0 (zip attackingDiceRolls defendingDiceRolls)

  return Battlefield { attackers = currAttackers - x, defenders = currDefenders - y}

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade currBf@(Battlefield a d) 
  | a < 2 || d == 0 = return currBf
  | otherwise = (battle currBf) >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do 
  results <- replicateM 1000 (invade bf)
  let successCount = length . filter (\bf -> (defenders bf) == 0) $ results
  return (fromIntegral successCount / 1000)

main = do
  randomDieVal <- evalRandIO die
  print randomDieVal

  values <- evalRandIO (dice 10)
  putStrLn (show values)

  print (Battlefield 1 2)
  print (map calculateMaxAttackers [-1.. 10])
  print (map calculateMaxDefenders [-1.. 10])

  nextBattlefield <- evalRandIO (battle (Battlefield 4 2))
  print nextBattlefield

  invadedBattleField <- evalRandIO (invade (Battlefield 100 5))
  print invadedBattleField

  prob <- evalRandIO (successProb (Battlefield 5 1))
  print prob
