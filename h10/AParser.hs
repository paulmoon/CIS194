{-# LANGUAGE InstanceSigs #-}

{- CIS 194 HW 10
{-# LANGUAGE InstanceSigs #-}

   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (Gthat is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
charParser :: Char -> Parser Char
charParser c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise #1
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  -- Q: Given a function (a -> b) and a (Parser a), how do you construct
  -- a (Parser b)? 
  -- A: Run the Parser on 'a', then run the function on the output of the Parser.
  fmap f parser = Parser $ fmap (first f) . runParser parser
  -- Parser $ fmap (first f) . runParser parser
  -- runParser parser :: String -> Maybe (a, String)
  -- f . g =  f (g (x))
  -- f . g :: (b -> c) -> (a -> b) -> (a -> c)

  -- fmap (first f) . runParser parser :: fmap (first f) . (String -> Maybe (a, String))
  -- ==> (String -> (first f) -> Maybe (a, String))
  -- ==> (String -> (Maybe f a, String))
  -- ==> (String -> (Maybe b, String))

-- Exercise #2
instance Applicative Parser where
  pure x       = Parser (\s -> Just (x, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2    = Parser $ \s -> case runParser p1 s of
    Nothing -> Nothing
    Just (p1_f, p1_rest) -> runParser (p1_f <$> p2) p1_rest

-- Exercise #3
abParser :: Parser (Char, Char)
-- Take Parser 'a', transform it into Parser 'b' only if the first one matches
-- (a -> b) -> f a -> f b
-- (\a b -> (a, b)) <$> charParser 'a' does the following:
-- Char -> (Char -> (Char, Char)) -> Parser Char -> Parser (Char -> (Char, Char))

-- then Parser (Char -> (Char, Char)) <*> charParser 'b' does the following:
-- Parser (Char -> (Char, Char)) -> Parser Char -> Parser (Char, Char), which is what we want!
abParser = (\a b -> (a, b)) <$> charParser 'a' <*> charParser 'b'

abParser_ :: Parser ()
abParser_  = (\a b -> ()) <$> charParser 'a' <*> charParser 'b'

intPair :: Parser [Integer] 
intPair = (\a _ b -> [a, b]) <$> posInt <*> charParser ' ' <*> posInt

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
-- (<*>) :: f (a -> b) -> f a -> f b  
-- f <$> x = fmap f x  
-- fmap :: (a -> b) -> f a -> f b

-- Exercise #4
instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> runParser p2 s
    something -> something

-- Exercise #5
intOrUpperCase :: Parser ()
intOrUpperCase = const () <$> posInt <|> const () <$> isUpperParser

isUpperParser :: Parser Char
isUpperParser = satisfy isUpper

main :: IO()
main = do 
  putStrLn "Hello World"

  print $ runParser abParser "abcdef"
  print $ runParser abParser "aebcdf"

  print $ runParser abParser_ "abcdef"
  print $ runParser abParser_ "aebcdf"

  print $ runParser intPair "12 34"

  print $ runParser intOrUpperCase "342abcd" == Just ((), "abcd")
  print $ runParser intOrUpperCase "XYZ" == Just ((), "YZ")
  print $ runParser intOrUpperCase "foo" == Nothing


