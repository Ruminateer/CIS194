module Hw2.LogAnalysis where

import Control.Exception
import Hw2.Log
import Text.Read

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E" : sv : t : msg)
    | Just time <- readMaybe t,
      Just severity <- readMaybe sv ->
        LogMessage (Error severity) time (unwords msg)
  ("I" : t : msg)
    | Just time <- readMaybe t ->
        LogMessage Info time (unwords msg)
  ("W" : t : msg)
    | Just time <- readMaybe t ->
        LogMessage Warning time (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMessage Leaf = Node Leaf newMessage Leaf
insert _ (Node _ (Unknown _) _) = throw (userError "MessageTree contains Unknown")
insert
  newMessage@(LogMessage _ newTime _)
  (Node lhs curMessage@(LogMessage _ curTime _) rhs)
    | newTime < curTime = Node (insert newMessage lhs) curMessage rhs
    | otherwise = Node lhs curMessage (insert newMessage rhs)

build :: [LogMessage] -> MessageTree
-- TODO: foldr vs foldl'?
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lhs message rhs) = inOrder lhs ++ message : inOrder rhs

isError50Plus :: LogMessage -> Bool
isError50Plus (LogMessage (Error severity) _ _) = severity >= 50
isError50Plus _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage (Unknown message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isError50Plus
