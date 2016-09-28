{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

------------------------------------------
-- Exercise 1
------------------------------------------

parseMessage :: String -> LogMessage

parseMessage msg = case words msg of
  ("I":ts:rest) -> LogMessage Info (read ts) (unwords rest)
  ("W":ts:rest) -> LogMessage Warning (read ts) (unwords rest)
  ("E":sevrty:ts:rest) -> LogMessage (Error (read sevrty)) (read ts) (unwords rest)
  _ -> Unknown msg

parse :: String -> [LogMessage]
parseLines :: [String] -> [LogMessage]

parse xs = parseLines (lines xs)
parseLines [] = []
parseLines (x:xs) = [parseMessage x] ++ parseLines xs

------------------------------------------
-- Exercise 2
------------------------------------------

insert :: LogMessage -> MessageTree -> MessageTree
timeStamp :: LogMessage -> TimeStamp

timeStamp (LogMessage _ timeStampValue _) = timeStampValue
timeStamp (Unknown _) = -1

insert (Unknown _) msgTree = msgTree
insert lgMsg Leaf = Node Leaf lgMsg Leaf
insert lgMsg (Node left b right)
  | timeStamp lgMsg == timeStamp b = Node left b right
  | timeStamp lgMsg < timeStamp b = Node (insert lgMsg left) b right
  | timeStamp lgMsg > timeStamp b = Node left b (insert lgMsg right)

------------------------------------------
-- Exercise 3
------------------------------------------

build :: [LogMessage] -> MessageTree

build (x:[]) = insert x Leaf
build xs = insert (head xs) (build (tail xs))

------------------------------------------
-- Exercise 4
------------------------------------------

inOrder :: MessageTree -> [LogMessage]

inOrder (Leaf) = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

------------------------------------------
-- Exercise 5
------------------------------------------

whatWentWrong :: [LogMessage] -> [String]
severity :: LogMessage -> Int
errorSeverity :: MessageType -> Int
filterMsgs :: [LogMessage] -> [LogMessage]
extractStrings :: [LogMessage] -> [String]
getString :: LogMessage -> String

getString (LogMessage _ _ str) = str
getString (Unknown str) = str

errorSeverity (Info) = -1
errorSeverity (Warning) = -1
errorSeverity (Error num) = num

severity (LogMessage tp _ _) = errorSeverity tp
severity (Unknown _) = -1

filterMsgs [] = []
filterMsgs (x:xs) =
  if severity x >= 50
    then
      [x] ++ filterMsgs xs
    else
      filterMsgs xs

extractStrings [] = []
extractStrings (x:xs) = [getString x] ++ extractStrings xs

whatWentWrong xs = extractStrings (inOrder (build (filterMsgs xs)))
