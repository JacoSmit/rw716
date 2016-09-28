module Test where

  -------------------------------------------------
  -- 1
  -------------------------------------------------

  data Tree a = Tree (Maybe ((Tree a),a,(Tree a))) deriving (Show, Eq)

  -------------------------------------------------
  -- 2
  -------------------------------------------------
  instance Functor Tree where
    --fmap f (Tree a b c) = Tree (fmap f a) (f b) (fmap f c)
    fmap f (Tree Nothing) = (Tree Nothing)
    fmap f (Tree (Just (a, b, c))) = Tree (Just ((fmap f a), (f b), (fmap f c)))

  -------------------------------------------------
  -- Examples
  -------------------------------------------------
  treeShow :: String
  treeShow = show tree1

  treeEqTrue :: Bool
  treeEqTrue = tree1 == tree1'

  treeEqFalse :: Bool
  treeEqFalse = tree1 == tree2

  tree1 = Tree (Just ((Tree (Just (Tree Nothing, 1, Tree Nothing))), 2, ((Tree (Just (Tree Nothing, 3, Tree Nothing))))))
  tree1' = Tree (Just ((Tree (Just ((Tree Nothing), 1, (Tree Nothing)))), 2, ((Tree (Just ((Tree Nothing), 3, Tree Nothing))))))
  tree2 = Tree (Just ((Tree (Just ((Tree Nothing), 1, (Tree Nothing)))), 2, ((Tree (Just ((Tree Nothing), 4, (Tree Nothing)))))))

  fmapExample :: Tree Integer
  fmapExample = fmap (+1) tree1

  -------------------------------------------------
  -- 3
  -------------------------------------------------

  treeInsert x (Tree Nothing) = Tree (Just ((Tree Nothing), x, (Tree Nothing)))
  treeInsert x (Tree (Just (a,b,c)))
    | x == b = Tree (Just (a,b,c))
    | x < b = Tree (Just (treeInsert x a, b, c))
    | x > b = Tree (Just (a, b, treeInsert x c))
  -- -------------------------------------------------
  -- -- Example
  -- -------------------------------------------------

  treeInsertExample = foldr treeInsert (Tree Nothing) [1..6]

  flatten (Tree Nothing) = []
  flatten (Tree (Just (l,x,r))) = flatten l ++ [x] ++ flatten r
