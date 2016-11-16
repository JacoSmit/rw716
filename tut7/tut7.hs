module Tut7 where
  import Control.Applicative

  solveRPN :: String -> Either String Float
  solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = ((*) <$> x <*> y):ys
            foldingFunction (x:y:ys) "+" = ((+) <$> x <*> y):ys
            foldingFunction (x:y:ys) "-" = ((-) <$> y <*> x):ys
            foldingFunction (x:y:ys) "^" = ((**) <$> y <*> x):ys
            foldingFunction (x:y:ys) "/" = division x y ys
            foldingFunction (x:xs) "ln" = logarithm x xs
            foldingFunction xs "sum" = [foldl (liftA2 (+)) (Right 0) xs]
            foldingFunction (x:xs) "*" = [Left "Too few numerical operands when applying the operator *"]
            foldingFunction (x:xs) "+" = [Left "Too few numerical operands when applying the operator +"]
            foldingFunction (x:xs) "-" = [Left "Too few numerical operands when applying the operator -"]
            foldingFunction (x:xs) "^" = [Left "Too few numerical operands when applying the operator ^"]
            foldingFunction (x:xs) "/" = [Left "Too few numerical operands when applying the operator /"]
            foldingFunction [] "ln" = [Left "Too few numerical operands when applying the operator ln"]
            foldingFunction [] "*" = [Left "Too few numerical operands when applying the operator *"]
            foldingFunction [] "+" = [Left "Too few numerical operands when applying the operator +"]
            foldingFunction [] "-" = [Left "Too few numerical operands when applying the operator -"]
            foldingFunction [] "^" = [Left "Too few numerical operands when applying the operator ^"]
            foldingFunction [] "/" = [Left "Too few numerical operands when applying the operator /"]
            foldingFunction xs numberString = Right (read numberString):xs

  division :: (Eq b, Fractional b) => Either String b -> Either String b -> [Either String b] -> [Either String b]
  division (Right 0) _ ys = Left "Division by zero":ys
  division x y ys = ((/) <$> y <*> x):ys

  --logarithm :: (Floating b, Ord b) => Either t b -> [Either String b] -> [Either String b]
  logarithm (Right x) xs = if x <= 0
    then Left "Log of non positive number encountered":xs
    else (log <$> Right x):xs
  logarithm (Left x) xs = [Left x]
