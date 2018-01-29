{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (\x y -> [x] ++ y) <$> p <*> zeroOrMore p <|> pure empty

-- (a -> a -> [a]) <$> Parser a <$> Parser [a] <|> Parser empty
-- Parser (a -> [a]) <$> Parser [a] <|> Parser empty
-- Parser (a -> [a]) <$> Parser [a] <|> Parser empty

-- zeroOrMore p = Parser ls 
--   where 
--     ls [] = Nothing
--     ls (x:xs) = case p x of 
--       Nothing -> Nothing
--       -- (Just (x_result, x_rest)) -> Just (x_result, x_
--       something -> something

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
-- (<*>) :: f (a -> b) -> f a -> f b  

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (\x y -> [x] ++ y) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (\x y -> [x] ++ y) <$> satisfy (isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (atomParser <|> multipleSExprParser) <* spaces
  where
    atomParser = A <$> (I <$> ident <|> N <$> posInt)
    multipleSExprParser = Comb <$> (char '(' *> zeroOrMore parseSExpr <* char ')')

main = do 
  putStrLn "Hello World"
  print $ runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
  print $ runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
  print $ runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
  print $ runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"

  print $ runParser ident "foobar baz" == Just ("foobar"," baz")
  print $ runParser ident "foo33fA" == Just ("foo33fA", "")
  print $ runParser ident "2bad" == Nothing
  print $ runParser ident "" == Nothing

  print $ runParser parseSExpr "5"
  print $ runParser parseSExpr "foo3"
  print $ runParser parseSExpr "(bar (foo) 3 5 874)"
  print $ runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
  print $ runParser parseSExpr "( lots of ( spaces in ) this ( one ) )"
