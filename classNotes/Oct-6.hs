module Oct6 where
  import Data.Monoid
  import Data.Foldable
  -- :m Data.Monoid
  --[1,2,3] `mappend` [4,5,6]
  --mconcat [[1,2], [3,4], [5,6]]
  --LT `mappend` GT
  --GT `mappend` LT
  -- EQ `mappend` LT
  -- mempty :: Ordering

  lengthComp ::String -> String -> Ordering
  lengthComp x y = (length x `compare` length y) `mappend` (x `compare` y)

  -- "zzz" `lengthComp` "aaaa"
  -- "zzzz" `lengthComp` "aaaa"

  data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

  instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

  instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

  testTree = Node 5
             (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
             )
             (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
             )

  -- getAny $ foldMap (\x -> Any $ x == 3) testTree
  -- Just 3 >>= (\x -> Just (x+1))
  -- Nothing >>= (\x -> Just (x+1))
  -- Just 0 >>= (\x -> Just (x+1)) >>= (\x -> Just (x+1))
  -- return 0 >>= (\x -> Just (x+1))
  -- return 0 >>= (\x -> Just (1/x))

  -- getLine >>= (\x -> return (x ++"!"))
  -- getLine >>= (\x -> (getLine >>= (\y -> return (x++y))))

  foo = do
    x <- getLine
    y <- getLine
    return (x++y)

  foo' = getLine >>= (\x ->
    getLine >>= (\y ->
      return (x++y)))
