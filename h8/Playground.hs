module Main where

-- putStrLn :: String -> IO ()

main :: IO()
main = do 
  putStrLn "Hello" >> putStrLn "world!"
  putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))


