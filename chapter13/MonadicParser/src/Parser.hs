module Parser where

import Control.Applicative
import Data.Char

-- type Parser a = String -> [(a, String)]

-- Dummy type in order to define classes
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P (\input -> case input of
                      [] -> []
                      (x:xs) -> [(x, xs)])

instance Functor Parser where
  fmap f p = P (\input -> case parse p input of
                            [] -> []
                            [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  pure v = P (\input -> [(v, input)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\input -> case parse pg input of
                             [] -> []
                             [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\input -> case parse p input of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

-- comsume 3 characters, discard second, return first and third as pair
-- using applicative
-- three :: Parser (Char, Char)
-- three = pure g <*> item <*> item <*> item
--   where g x y z = (x, z)

-- using monad(with do notation)
three :: Parser (Char, Char)
three = do x <- item
           item
           z <- item
           return (x, z)

instance Alternative Parser where
  empty = P (\_ -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
                           [] -> parse q input
                           [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol "," 
                         natural)
          symbol "]"
          return (n:ns)
