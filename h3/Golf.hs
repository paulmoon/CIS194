module Golf where

import Data.Char
import Data.List

-- Exercise 1: Hopscotch
-- "ABCD" == ["ABCD", "BD", "C", "D"]
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\(n, list) -> every n list) (zip [1..length xs] (repeat xs))
  
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []

-- Exercise 2: Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x, y] = []
localMaxima list@(x:y:z:_) = (if (y > x) && (y > z) then [y] else []) ++ localMaxima (drop 1 list)

-- Exercise 3: Histogram
histogram :: [Integer] -> String
histogram xs =
  -- For each number, build a string according to the occurrence e.g.
  -- [*, *] for 0 if 0 appears twice.
  -- Possibly pad the beginning of each string to match the number with max occurrence
  -- Then zip all results and join.
  let num_occurrences = map (\xs -> (head xs, length xs)) $ group $ sort xs
      bucket_strings = [case result of
            Just n -> take n $ repeat "*"
            Nothing -> [] 
          | n <- [0..9], let result = lookup n num_occurrences]
      padded_bucket_strings = 
        let max = maximum (map length bucket_strings)
        in map (\bucket -> replicate (max - (length bucket)) " " ++ bucket) bucket_strings
      footer = intercalate "\n" [(take 10 $ repeat '=') , map intToDigit [0..9] ]
    in intercalate "\n" (map (intercalate "") $ transpose padded_bucket_strings) ++ "\n" ++ footer ++ "\n"

main :: IO()
main = do 
  print $ skips "ABCD" == ["ABCD", "BD", "C", "D"]
  print $ skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
  print $ skips [1] == [[1]]
  print $ skips [True,False] == [[True,False], [False]]
  -- print $ skips [] == []  

  print $ localMaxima [2,9,5,6,1] == [9,6]
  print $ localMaxima [2,3,4,1,5] == [4]
  print $ localMaxima [1,2,3,4,5] == []

  putStr $ histogram [1,1,1,5]
  putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
