{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage string = let wordList = words string in
                      case wordList of
                        ("I":timestamp:msg) -> LogMessage Info (read timestamp) (unwords msg)
                        ("W":timestamp:msg) -> LogMessage Warning (read timestamp) (unwords msg)
                        ("E":severity:timestamp:msg) -> LogMessage (Error (read severity)) (read timestamp) (unwords msg)
                        _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

-- printFile :: FilePath -> IO String
-- printFile file = readFile file

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg@(LogMessage _ msgTs _) tree = case tree of
  Leaf -> Node Leaf msg Leaf
  (Node left curr@(LogMessage _ currTs _) right) -> 
    if msgTs < currTs
      then (Node (insert msg left) curr right)
      else (Node left curr (insert msg right))
  (Node _ (Unknown _) _) -> error "Unknown message should not be in a MessageTree!"

-- Exercise 3
build :: [LogMessage] -> MessageTree
build messages = buildHelper messages Leaf

buildHelper :: [LogMessage] -> MessageTree -> MessageTree
buildHelper [] tree = tree
buildHelper (x:xs) tree = buildHelper xs (insert x tree)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong list = map getMessage $ filter msgIsImportant list

getMessage :: LogMessage -> String
getMessage (Unknown msg) = msg
getMessage (LogMessage _ _ msg) = msg

msgIsImportant :: LogMessage -> Bool
msgIsImportant (Unknown _) = False
msgIsImportant (LogMessage t _ _) = case t of
  (Error severity) -> severity >= 50
  _ -> False

main :: IO()
main = do
  -- print (parseMessage "E 2 562 help help")
  -- print (parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help")
  -- print (parseMessage "I 29 la la la" == LogMessage Info 29 "la la la")
  -- print (parseMessage "This is not in the right format" == Unknown "This is not in the right format")
  -- msg <- testParse parse 10 "error.log"
  -- putStr $ unlines $ map show msg

  -- contents <- readFile "error.log"
  -- putStr $ unlines (map show $ parse contents)

  -- putStr $ unlines $ (map show $ inOrder (build $ parse contents))
  -- putStrLn "Hello world"

  results <- testWhatWentWrong parse whatWentWrong "error.log"
  putStrLn $ unlines $ (map show results)
