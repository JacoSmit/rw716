module Test where
  import Data.Array

  -- Jaco Smit, 17523559

  -- Section A
  --------------------------------------------
  --Question 1:
  --------------------------------------------
  --Since Haskell is lazy, it simply pushes each instruction on the stack and then only when it needs it, will it start evaluating it. --This means that there will be pushed 1000000000 or slightly larger (+1) evaluations.

  --a) Haskell has a built-in foldl' and foldr' function in the Data.List package. These functions evaluate each instruction rather than pushing the instructions on the stack.

  --b) There is a function called seq which evaluates the instruction immediately. This means that implementing your own foldl function can be accomplished by evaluating each instruction at each step of the foldl action.

  --------------------------------------------
  --Question 2:
  --------------------------------------------
  --When popping off a value of the Queue, the head will be removed. This means that all the elements the list should be shifted up to the front. This is very inefficient for lists in Haskell since all the remaining elements in the list needs to be moved and thus visited.

  -- Section B
  --------------------------------------------
  -- Question 1:
  --------------------------------------------
  fibArr n = m ! n where
    m = array (0, n) $
      [(0, 1)] ++ [(1,1)] ++
      [(i,val)
        | i <- [2 .. n]
        , let val = m ! (i-1) + m ! (i-2)
      ]

  fib n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = fibArr n

  fibEX1 = fib 5
  fibEX2 = fib 20

  -- Use of arrays enables the linear algorithmic time constraint.

  --------------------------------------------
  -- Question 2:
  --------------------------------------------
  squareFn f = (** 2.0) .f

  squareFnEX1_g = squareFn (*2.0)
  squareFnEX1 = squareFnEX1_g 2

  --------------------------------------------
  -- Question 3:
  --------------------------------------------

  mapEach :: (a -> b) -> [[a]] -> [[b]]
  mapEach f xss = map (fmap f ) xss

  mapEachEx1 = mapEach (+2) [[5, 4, 1], [7, 6], [8, 12]]
  mapEachEx2 = mapEach (> 3) [[1, 7], [0, 4], [9, 10], []]
  mapEachEx3 = mapEach (\y -> y*y) [[0, 4], [9, 10, 2, 7, 5], [1 .. 3]]
  mapEachEx4 = mapEach (+3) []
  mapEachEx5 = mapEach (+4) [[]]
