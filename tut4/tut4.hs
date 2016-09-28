module Tut4 where

  ----------------------------------
  --    Exercise 1
  ----------------------------------

  -- Orignal definitions
  fun1 :: [Integer] -> Integer
  fun1 [] = 1
  fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

  fun2 :: Integer -> Integer
  fun2 1 = 0
  fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

  -- Recoded implementations
  fun1' :: [Integer] -> Integer
  fun1' = foldl (\x xs -> (xs-2) * x) 1 . filter even

  fun2' :: Integer -> Integer
  fun2' = sum . filter even . takeWhile (/= 1) . iterate f where
    f 1 = 0
    f n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

  ----------------------------------
  --    Exercise 2
  ----------------------------------

  data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

  foldTree :: [a] -> Tree a
  foldTree = foldr insert Leaf

  insert :: t -> Tree t -> Tree t
  insert x Leaf = Node 0 Leaf x Leaf
  insert x (Node depth left a right)
    | height left < height right = Node (depth) (insert x left) a right
    | height left > height left = Node (depth) left a (insert x right)
    | height (insert x left) < height (insert x right) = Node (depth) (insert x left) a right
    | otherwise = Node (height (insert x right) + 1) left a (insert x right)

  height :: Tree t -> Integer
  height (Node depth _ _ _) = depth
  height Leaf = 0
