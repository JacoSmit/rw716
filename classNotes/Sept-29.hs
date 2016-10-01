module Sept29 where
  import Control.Applicative

  f1 = (+) <$> Just 3 <*> Just 4
  f2 = Just (+) <*> Just 3 <*> Just 4
  f3 = fmap (+) (Just 3) <*> Just 4

  f4 = (+) <$> [1, 2,3] <*> [4,5,6]
  f5 = (+) <$>  ZipList  [1,2,3] <*> ZipList [4,5,6]
  f6 = getZipList f5

  newtype ZL a = ZL {getL :: [a]}

  f7 = getL $ ZL [1, 2, 3]
  f8 = (++) <$> getLine <*> getLine
  f9 = (+) <$> (*2) <*> (*4) $ 5

  f10 = liftA2 (+) (Just 2) (Just 3)
  f11 = liftA2 (:) (Just 2) (Just [3,4])
  f12 = liftA2 (:) [2,3] [[1,2]]
  f13 = liftA2 (:) [2,3] [[1,2], [4,5]]

  f14 = pure 4 :: [Int]
  f15 = pure 4 :: Maybe Int

  f16 = liftA2 (:) (Just 3) (pure [])
  f17 = liftA2 (:) (Just 4) (Just [3])

  f18 = sequence [Just 1, Just 2, Just 3]
  f19 = sequence [getLine, getLine]
  --f20 = sequence [ZipList [1,2,3], ZipList [4,5,6]]
  f21 = sequence [[1,2,3]]
  f22 = liftA2 (:) [1,2,3] (pure [])

  f23 = (:) <$> [1,2,3] <*> [[]]
  f24 = (:) <$> [1,2,3] <*> pure []

  t = liftA2 (:) [1,2,3] [[]]
  f25 = liftA2 (:) [4,5,6] t
  f26 = sequence [[1,2,3]]
  f27 = sequence [(>3), (<11), odd] 5

  f28 = and f27

  newtype Pair a b = Pair {getT :: (b,a)} deriving (Show)
  instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)
