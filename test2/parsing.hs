module Parsing where

import Control.Applicative
import Control.Monad
import Data.Char

infixr 5 +++
newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
    return v = P (\inp -> [(v,inp)])
    p >>= f = P (\inp ->
                     case parse p inp of
                       [(v, out)] -> parse (f v) out
                       [] -> [])

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where
    mzero                      =  P (\inp -> [])
    p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])
-- Basic parser
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                              []     -> []
                                              (x:xs) -> [(x,xs)])

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q


sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many'                          :: Parser a -> Parser [a]
many' p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many' p
                                    return (v:vs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  (do char '-'
                                     n <- nat
                                     return (-n))
                                  +++ nat


space                         :: Parser ()
space                         =  do many' (sat isSpace)
                                    return ()
