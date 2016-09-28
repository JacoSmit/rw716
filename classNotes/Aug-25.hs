module Aug25 where

  -- :info []
  -- :k Int or Maybe (kind of maybe)
  -- * = Concrete type
  -- :t Left (10::Int)
  -- :t Right "Hello"
  -- :k Either
  -- :i Functor

  -- Own Functor class
  class Func f where
    fmap' :: (a -> b) -> (f a -> f b)

  instance Func Maybe where
    fmap' g Nothing = Nothing
    fmap' g (Just x) = Just (g x)

  -- fmap' (*18) (Just 10)
  -- fmap (*18) (Just 10)
  -- g can only work on one type either a or b thus just return Left x. Only gonna apply g on Right
  instance Func (Either a) where
    fmap' g (Left x) = Left x
    fmap' g (Right x) = Right (g x)

  -- fmap' (*10) (Right 10)
  -- fmap' (*10) (Left 10)

  data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)
  leaf x = Node Empty x Empty

  -- :k Tree

  instance Func Tree where
    fmap' g Empty = Empty
    fmap' g (Node l x r) = Node (fmap' g l) (g x) (fmap' g r)

  -- let x = Node (Node Empty 10 Empty) 20 (Node Empty 30 Empty)
  -- fmap' (+2) x

  
