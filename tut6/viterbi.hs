module Viterbi where
  import Data.Array
  import Data.List

  data Hmm a b = Hmm { states :: [a],
                       observations :: [b],
                       initPi :: a -> Double, -- Initial Probabilities
                       transM :: a -> a -> Double, -- Transition Matrix function
                       emmM :: a -> b -> Double -- Emmision Matrix function
                     }

  viterbi :: Hmm a b -> [b] -> [a]
  viterbi hmm obs = backtrack hmm obs $ buildTable hmm obs

  backtrack :: Ord a1 => Hmm t t1 -> [a] -> (Array (Int, Int) a1, Array (Int, Int) Int) -> [t]
  backtrack (Hmm states obsSpace initPi transM emmM) obs (t1, t2)  = xs where
    k = length states - 1
    t = length obs - 1
    zT = maxIndex $ getMaxIndex [t1 ! (r, t) | r <- [0 .. k]]
    xs = backtrackAppend t2 t states zT t [states !! zT]

  backtrackAppend :: (Num a, Ix a) => Array (Int, a) Int -> t -> [a1] -> Int -> a -> [a1] -> [a1]
  backtrackAppend _ _ _ _ 0 xs = xs
  backtrackAppend t2 t states zCur i xs = backtrackAppend t2 t states zPrev (i-1) (x:xs) where
    zPrev = t2 ! (zCur, i)
    x = states !! zPrev

  buildTable :: Hmm a b -> [b] -> (Array (Int, Int) Double, Array (Int, Int) Int)
  buildTable (Hmm states obsSpace initPi transM emmM) obs = (t1, t2) where
    k = length states - 1
    t = length obs - 1
    t1 = array ((0,0), (k, t)) $
      [((i,0), initPi (states !! i) * emmM (states !! i) (head obs)) | i <- [0 .. k]] ++ -- initialization step
      [((j,i), best)
        | i <- [1 .. t]
        , j <- [0 .. k]
        , let best = maximum [(t1 ! (r, i-1)) * transM (states !! r) (states !! j) * emmM (states !! j) (obs !! i) | r <- [0 .. k]]
      ]
    t2 = array ((0,0), (k, t)) $
      [((i,0), 0) | i <- [0 .. k]] ++ -- initialization step
      [((j,i), best)
        | i <- [1 .. t]
        , j <- [0 .. k]
        , let best = maxIndex $ getMaxIndex [(t1 ! (r, i-1)) * transM (states !! r) (states !! j) * emmM (states !! j) (obs !! i) | r <- [0 .. k]]
      ]

  getMaxIndex :: Ord a => [a] -> Maybe Int
  getMaxIndex xs = elemIndex (maximum xs) xs

  maxIndex :: Num t => Maybe t -> t
  maxIndex (Just x) = x
  maxIndex Nothing = -1

  -- WikiPedia Example
  wikiHmm = Hmm { states = ["Healthy", "Fever"],
                  observations = ["normal", "cold", "dizzy"],
                  initPi = wikiPi,
                  transM = wikiTrans,
                  emmM = wikiEmm
                }
  wikiPi s
    | s == "Healthy" = 0.6
    | s == "Fever" = 0.4

  wikiTrans s1 s2
    | s1 == "Healthy" && s2 == "Healthy" = 0.7
    | s1 == "Healthy" && s2 == "Fever" = 0.3
    | s1 == "Fever" && s2 == "Healthy" = 0.4
    | s1 == "Fever" && s2 == "Fever" = 0.6

  wikiEmm state obs
    | state == "Healthy" && obs == "normal" = 0.5
    | state == "Healthy" && obs == "cold" = 0.4
    | state == "Healthy" && obs == "dizzy" = 0.1
    | state == "Fever" && obs == "normal" = 0.1
    | state == "Fever" && obs == "cold" = 0.3
    | state == "Fever" && obs == "dizzy" = 0.6

  runWikiExample = viterbi wikiHmm ["normal", "cold", "dizzy"]

  -- Another Example

  anotherHmm = Hmm { states = ["Sunny", "Cloudy", "Rainy"],
                     observations = ["Dry", "Dryish", "Damp", "Soggy"],
                     initPi = anotherPi,
                     transM = anotherTrans,
                     emmM = anotherEmm
                   }
  anotherPi s
    | s == "Sunny" = 0.63
    | s == "Cloudy" = 0.17
    | s == "Rainy" = 0.20

  anotherTrans s1 s2
    | s1 == "Sunny" && s2 == "Sunny" = 0.5
    | s1 == "Sunny" && s2 == "Cloudy" = 0.25
    | s1 == "Sunny" && s2 == "Rainy" = 0.25
    | s1 == "Cloudy" && s2 == "Sunny" = 0.357
    | s1 == "Cloudy" && s2 == "Cloudy" = 0.125
    | s1 == "Cloudy" && s2 == "Rainy" = 0.375
    | s1 == "Rainy" && s2 == "Sunny" = 0.125
    | s1 == "Rainy" && s2 == "Cloudy" = 0.675
    | s1 == "Rainy" && s2 == "Rainy" = 0.375

  anotherEmm state obs
    | state == "Sunny" && obs == "Dry" = 0.6
    | state == "Sunny" && obs == "Dryish" = 0.2
    | state == "Sunny" && obs == "Damp" = 0.15
    | state == "Sunny" && obs == "Soggy" = 0.05
    | state == "Cloudy" && obs == "Dry" = 0.25
    | state == "Cloudy" && obs == "Dryish" = 0.25
    | state == "Cloudy" && obs == "Damp" = 0.25
    | state == "Cloudy" && obs == "Soggy" = 0.25
    | state == "Rainy" && obs == "Dry" = 0.05
    | state == "Rainy" && obs == "Dryish" = 0.1
    | state == "Rainy" && obs == "Damp" = 0.35
    | state == "Rainy" && obs == "Soggy" = 0.5

  runAnotherExample = viterbi anotherHmm ["Dry", "Dryish", "Damp", "Soggy"]
