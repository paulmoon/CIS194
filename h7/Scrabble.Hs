{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Char

import JoinList

-- Exercise 3
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c 
  | upperC `elem` "AEILNORSTU" = Score 1
  | upperC `elem` "GD" = Score 2
  | upperC `elem` "BCMP" = Score 3
  | upperC `elem` "FHVWY" = Score 4
  | upperC == 'K' = Score 5
  | upperC `elem` "JX" = Score 8
  | upperC `elem` "QZ" = Score 10
  | otherwise = Score 0
  where upperC = toUpper c

scoreString :: String -> Score
scoreString = sum . map score

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreToInt :: Score -> Int
scoreToInt (Score a) = a

-- main :: IO()
-- main = do 
--   print $ scoreString "yay "
--   print $ scoreString "haskell!"
--   print $ scoreLine "yay " +++ scoreLine "haskell!" == 
--     Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")

