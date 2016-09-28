module Sept15 where
  calcRPN :: String -> Double

  calcRPN input = head $ foldl foldF [] $ words input where
    foldF (x:y:xs) "+" = (x+y):xs
    foldF (x:y:xs) "-" = (y-x):xs
    foldF (x:y:xs) "*" = (x*y):xs
    foldF (x:y:xs) "/" = (y/x):xs
    foldF (x:y:xs) "**" = (y**x):xs
    foldF xs "sum" = [sum xs]
    foldF xs num = read num : xs -- need to use read since otherwise you are pushing characters


  -- calcRPN "2 2 2 sum 2 **"
  -- issue: calcRPN "2 4 8 +" -- still elements left on stack

  -- Viterbi do find shortest path
  data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

  -- s = Section 50 10 30
  -- getC s

  type RoadSystem = [Section]
  data Label = A | B | C deriving (Show)
  type Path = [(Label, Int)]

  --London to Heathrow
  l2h :: RoadSystem
  l2h = [Section 50 10 30,
         Section 5 90 20,
         Section 40 2 25,
         Section 10 8 0]

  roadStep :: (Path, Path) -> Section -> (Path, Path)
  roadStep (pathA, pathB) (Section a b c) =
    let lengthA = sum $ map snd pathA
        lengthB = sum $ map snd pathB
        f2A = lengthA + a
        c2A = lengthB + b + c
        f2B = lengthB + b
        c2B = lengthA + a + c
        newA = if f2A <= c2A
          then (A, a):pathA
          else (C, c):(B, b):pathB
        newB = if f2B <= c2B
          then (B, b):pathB
          else (C, c):(A, a):pathA
    in (newA, newB)

-- r1 = roadStep ([],[]) (Section 50 10 30)
-- r2 = roadStep r1 (Section 5 90 20)
-- foldl roadStep ([], []) l2h
