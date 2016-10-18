module Oct13 where
  f1 = [1,2,3] >>= \x -> [x,-x]
  f2 = [1,2]>>= \x -> ['a', 'b'] >>= \n -> [(x, n)]

  foo = do
    x <- [1,2]
    n <- ['a', 'b']
    return (x,n)

  type KP = (Int, Int) --Knight position

  km :: KP -> [KP] -- Knight move
  km (c,r) = filter onBoard -- column, row
    [(c+2, r+1), (c+2, r-1), (c-2, r+1), (c-2, r-1),
     (c+1, r+2), (c+1, r-2), (c-1, r+2), (c-1, r-2)]
     where
       onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

  -- km (1,1)
  -- [(1,1)] >>= km >>= km >>= km

  in3 :: KP -> [KP]
  in3 start = do
    first <- km start
    second <- km first
    km second

  -- in3 (1,1)

  canR :: KP -> KP -> Bool-- can reach
  canR from to = to `elem` in3 from

  type Birds = Int
  type Pole = (Birds, Birds)

  landLeft :: Birds -> Pole -> Maybe Pole
  landLeft n (l, r)
    | abs(l+n-r) < 4 = Just (l+n,r)
    | otherwise = Nothing

  landRight:: Birds -> Pole -> Maybe Pole
  landRight n (l, r)
    | abs(r+n-l) < 4 = Just (l,r+n)
    | otherwise = Nothing

  -- return (0,0) >>= landLeft 2 >>= landRight 5
  -- return (0,0) >>= landLeft 2 >>= landRight 6
  -- return (0,0) >>= landLeft 2 >>= landRight 6 >>= landRight (-3)

  banana = \_ -> Nothing

  -- return (0,0) >>= landLeft 2 >>= banan >>= landRight 3

  routine :: Maybe Pole
  routine = do
    step0 <- return (0,0)
    step1 <- banana step0
    step2 <- landLeft 2 step1
    landRight 4 step2

  -- return (0,0) >>= landLeft 2 >>= Nothing >>= landRight 3
  -- return (0,0) >>= landLeft 2 >>= Just (10,10) >>= landRight 3

  routine2 :: Maybe Pole
  routine2 = do
    step0 <- return (0,0)
    Nothing
    landLeft 2 step0


  routine3 :: Maybe Pole
  routine3 = do
    step0 <- return (0,0)
    Just (10,10)
    landLeft 2 step0

  otherRoutine =
    return (0,0) >>= \step0 -> Just (10,10) >> landLeft 2 step0

  foo2 = do
    x <- Just 10
    y <- Just "!"
    return $ show x ++ y

  bar2 = Just 10 >>= \x -> Just "!" >>= \y -> return $ show x ++ y

  bar3 = Just 10 >>= \x -> (Just "!" >>= \y -> return $ show x ++ y)
