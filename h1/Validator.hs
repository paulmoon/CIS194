
{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits(div n 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev(div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : 2 * y : doubleEveryOther(zs)

sumDigits :: [Integer] -> Integer
sumDigits list = sum $ map (sum . toDigits) list

validate :: Integer -> Bool
validate n = if mod (sumDigits . doubleEveryOther $ toDigitsRev n) 10 == 0 then True
             else False

main :: IO()
main = do
  print (toDigits 1111111221)
  print (toDigitsRev 139853915)
  print (doubleEveryOther $ toDigitsRev 139853915)
  print (doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5])

  print (sumDigits [16, 7, 12, 5])
  print (validate 4012888888881881)
  print (validate 4012888888881882)
