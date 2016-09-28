module Aug25IO where

  -- main
  -- runhaskell Aug-25-IO
  main = do
    line <- getLine
    if null line
      then
        return ()
      else
        do
          putStrLn (reverseWords line)
          main

  reverseWords = unwords . map reverse . words
