module Golf where
  import Data.List
  -----------------------------
  -- Exercise 1
  -----------------------------
  skips :: [a] -> [[a]]

  skips xs
    | null xs = []
    | otherwise = map f [1..n] where
        n = length xs
        f = every xs

  every xs n = case drop (n-1) xs of
              (y:ys) -> y : every ys n
              [] -> []

  -----------------------------
  -- Exercise 2
  -----------------------------
  localMaxima :: [Integer] -> [Integer]

  localMaxima [a] = []
  localMaxima [a,b] = []
  localMaxima (a:b:c:ds) = if b > a && b > c
    then
      b : localMaxima (b:c:ds)
    else
      localMaxima (b:c:ds)

  -----------------------------
  -- Exercise 3
  -----------------------------
  histogram :: [Int] -> String

  count :: Eq a => a -> [a] -> Int
  counts :: Eq a => [a] -> [a] -> [Int]
  horizHistogram :: [Int] -> [String]
  pad :: [String] -> [String]
  verticHistogram :: [Int] -> [String]
  stringify :: [String] -> String

  count x = length . filter (==x)

  -- Counts the number of times the value occurs
  -- vals = [0..9] (the entries in the histogram)
  -- xs = the input
  counts vals xs = map (`count` xs) vals

  -- Replicates * for the nu,ber in the given list
  horizHistogram = map (`replicate` '*')

  pad rows = map (take width) infRows -- Only takes the maximum number of values of teh padded entries.
    where
      width = maximum $ map length rows -- Gets the maxium length of each string in the list
      infRows = map (++ repeat ' ') rows -- Pads each entry to an infinite list

  verticHistogram = reverse . transpose . pad . horizHistogram

  stringify [] = ""
  stringify [x] = x
  stringify (x:xs) = x ++ "\n" ++ stringify xs

  histogram xs = stringify (verticHistogram (counts vals xs)) ++ legend
    where
      legend = "\n" ++ map (const '=') vals ++ "\n" ++ concatMap show vals ++ "\n"
      vals = [0..9]
