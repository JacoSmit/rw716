--main = do
--  line <- fmap reverse getLine
--  putStr line

  -- runhaskell Sept-22-IO.hs

main = do
  x <- (++) <$> getLine <*> getLine
  putStrLn x
