data FailableDouble = Failure
                    | OK Double
  deriving (Eq, Show)

failureToZero :: FailableDouble -> Double
failureToZero n = case n of
                    Failure -> 0
                    OK d    -> d

n = Failure
m = OK 2

main :: IO()
main = do
  -- print one
  print n
  print m
  print $ failureToZero n
  print $ failureToZero m
  print $ unwords $ words "hello world"