{-# OPTIONS_GHC -Wall #-}

module Hw2.LogAnalysis where

import Hw2.Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E" : severity : t : msg) -> LogMessage (Error (read severity)) (read t) (unwords msg)
  ("I" : t : msg) -> LogMessage Info (read t) (unwords msg)
  ("W" : t : msg) -> LogMessage Warning (read t) (unwords msg)
  _ -> Unknown s
