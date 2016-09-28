module Tut7 where
  import Data.List
  import Control.Applicative
  import Data.Either

  solveRPN :: String -> Either String Float
  solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = ((*) <$> x <*> y):ys
            foldingFunction (x:y:ys) "+" = ((+) <$> x <*> y):ys
            foldingFunction (x:y:ys) "-" = ((-) <$> y <*> x):ys
            foldingFunction (x:y:ys) "^" = ((**) <$> y <*> x):ys
            foldingFunction (x:y:ys) "/" = division x y ys
            foldingFunction (x:xs) "ln" = logarithm x xs
            foldingFunction xs "sum" = [Right (sum  (rights xs))]
            foldingFunction xs numberString = Right (read numberString):xs  

  division :: (Eq b, Fractional b) => Either String b -> Either String b -> [Either String b] -> [Either String b]
  division (Right 0) _ ys = Left "Division by zero":ys
  division x y ys = ((/) <$> y <*> x):ys

  logarithm :: (Floating b, Ord b) => Either t b -> [Either String b] -> [Either String b]
  logarithm (Right x) xs = if x <= 0
    then Left "Log of non positive number encountered":xs
    else (log <$> Right x):xs
