module Calc where
  import ExprT
  import Parser (parseExp)

  ----------------------------------
  --      Exercise 1              --
  ----------------------------------

  eval :: ExprT -> Integer
  eval (Add exp1 exp2) = eval exp1 + eval exp2
  eval (Mul exp1 exp2) = eval exp1 * eval exp2
  eval (Lit x) = x

  ----------------------------------
  --      Exercise 2              --
  ----------------------------------

  evalStr :: String -> Maybe Integer
  evalStr s = case parseExp Lit Add Mul s of
    Just r -> Just (eval r)
    Nothing -> Nothing

  --evalStr s = check (parseExp Lit Add Mul s)

  --check :: Maybe ExprT -> Maybe Integer
  --check Nothing = Nothing
  --check (Just a) = Just (eval a)

  ----------------------------------
  --      Exercise 3              --
  ----------------------------------

  class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

  instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

  reify :: ExprT -> ExprT
  reify = id

  ----------------------------------
  --      Exercise 4              --
  ----------------------------------

  data MinMax = MinMax Integer deriving (Eq, Show, Ord)
  data Mod7 = Mod7 Integer deriving (Eq, Show)

  instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

  instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

  instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

  instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

  testExp :: Expr a => Maybe a
  testExp = parseExp lit add mul "(3 * -4) + 5"
  testInteger = testExp :: Maybe Integer
  testBool = testExp :: Maybe Bool
  testMM = testExp :: Maybe MinMax
  testSat = testExp :: Maybe Mod7
