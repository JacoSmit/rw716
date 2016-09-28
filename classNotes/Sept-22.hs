module Sept22 where
  data MaybeC a = NothingC | JustC Int a deriving (Show)

  instance Functor MaybeC where
    fmap f NothingC = NothingC
    fmap f (JustC counter x) = JustC (counter+1) (f x)


  -- let t = JustC 0 "Hello"
  -- fmap id t
  -- fmap (*2) $ fmap (+3) [1,2,3]
  -- fmap (\x -> (x+3)*2) [1,2,3]


  -- :m Control.Applicative
  -- pure (+2) <*> Just 5
  -- pure (+) <*> Just 5 <*> Just 7
  -- (+) <$> Just 5 <*> Just 7
  -- pure (++) <*> Just "jonta" <*> Just "volta"
  -- (++) <$> Just "jonta" <*> Just "volta"
  -- (++) <$> Nothing <*> Just "volta" --Concatenate Maybe Strings
  -- fmap (++) (Just "jonta") <*> Just "volta"

  --[(+2), (+3)] <*> [4,5,6]
  -- (+) <$> [1,2] <*> [3,4]

  -- zipWidth (+) [1,2,3] [4,5,6]
  -- zipWith (+) [1,2,3,4,5] [4,5,6]
  -- zipWith (\x y -> (x,y)) [1,2,3,4,5] [4,5,6]
  -- zipWith3 (\x y z-> (x,y,z)) [1,2,3,4,5] [4,5,6] [7,8]
  -- zipWith (,) [1,2,3,4,5] [4,5,6]
  -- zipWith3 (,,) [1,2,3,4,5] [4,5,6]
  -- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [4,5,6]
  -- getZipList $ (,,) <$> ZipList "Hello" <*> ZipList "There" <*> ZipList "bye"
