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

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _ ) = ts
getTimeStamp (Unknown _) = undefined 



insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node mtl msg mtr)
  | getTimeStamp lm < getTimeStamp msg = Node (insert lm mtl) msg mtr
  | getTimeStamp lm >= getTimeStamp msg = Node mtl msg (insert lm mtr)
insert (LogMessage _ _ _) (Node _ _ _) = error "Unreachable pattern"

-- Exercise 3 --
build :: [LogMessage] -> MessageTree
build msgs = go msgs Leaf
  where go :: [LogMessage] -> MessageTree -> MessageTree
        go [] tree = tree
        go (x:xs) tree = go xs (insert x tree)

-- Exercise 4 --
inOrder :: MessageTree -> [LogMessage]
inOrder mt = go mt []
  where go :: MessageTree -> [LogMessage] -> [LogMessage]
        go Leaf list = list
        go (Node mtl msg mtr) list = go mtr (go mtl list ++ [msg])

-- Exercise 5 --
importantError :: LogMessage -> Bool
importantError (LogMessage (Error n) _ _) = n >= 50
importantError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map show $ filter importantError (inOrder $ build messages)

