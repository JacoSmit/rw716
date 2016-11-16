module Eval where
  import Data.Char
  import Control.Applicative
  import Control.Monad

  -- Jaco Smit, 17523559

  expr :: Parser Double
  expr = do t <- term
            (do space
                char '+'
                e <- expr
                return (t+e))+++ return t

  term :: Parser Double
  term = (do space
             string "factorial"
             space
             f <- factor
             return (factorial f)) +++ (do space
                                           string "log"
                                           space
                                           f <- factor
                                           return (log f)) +++ (do space
                                                                   string "exp"
                                                                   space
                                                                   f <- factor
                                                                   return (exp f))+++(do f <- factor
                                                                                         space
                                                                                         char '*'
                                                                                         t <- term
                                                                                         return (f*t)) +++ (do f <- factor
                                                                                                               return f)


  factor :: Parser Double
  factor = do space
              d <- double
              return d
              +++ (do space
                      char '('
                      e <- expr
                      space
                      char ')'
                      return e)

  eval xs = fst (head (parse expr xs))

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

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

  digitOrPoint                  :: Parser Char
  digitOrPoint                  =  sat pointOrDigit


  double                        :: Parser Double
  double                        = do xs <- many1 digitOrPoint
                                     return (read xs)

  pointOrDigit :: Char -> Bool
  pointOrDigit x = if isDigit x || x=='.'
    then True
    else False

  ex1 = eval "2*(3+4)"
  ex2 = eval "2    *    (    3   +    4   )"
  ex3 = eval "2 * ( 3  +  4.2)"
  ex4 = eval "2 * ( 3  +  14.2)"
  ex5 = eval "2 * ( 3  +  factorial 4)"
  ex6 = eval "2 * ( 3  +  exp 4.2)"
  ex7 = eval "2 * ( 3  +  log 4.5)"
