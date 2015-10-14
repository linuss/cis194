{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1 --
parseMessage :: String -> LogMessage
parseMessage s = go (words s) 
  where go :: [String] -> LogMessage
        go ("E":x:y:xs) = LogMessage (Error (read x)) (read y) (unwords xs)
        go ("I":x:xs) = LogMessage Info (read x) (unwords xs)
        go ("W":x:xs) = LogMessage Warning (read x) (unwords xs)
        go xs = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2 --

getLogMessage :: MessageTree -> LogMessage
getLogMessage (Node _ msg _) = msg
getLogMessage Leaf = Unknown

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _ ) = ts
getTimeStamp (Unknown _) = undefined 



insert :: LogMessage -> MessageTree -> MessageTree
insert Unknown mt = mt
insert lm (Node mtl msg mtr)
  | getTimeStamp lm > 

