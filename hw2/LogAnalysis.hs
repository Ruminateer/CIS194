{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Test.HUnit

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E" : severity : t : msg) -> LogMessage (Error (read severity)) (read t) (unwords msg)
  ("I" : t : msg) -> LogMessage Info (read t) (unwords msg)
  ("W" : t : msg) -> LogMessage Warning (read t) (unwords msg)
  _ -> Unknown s

testParseMessage :: Test
testParseMessage =
  TestList
    [ TestCase
        ( assertEqual
            "parseMessage E 2 562 help help"
            (LogMessage (Error 2) 562 "help help")
            (parseMessage "E 2 562 help help")
        ),
      TestCase
        ( assertEqual
            "parseMessage I 29 la la la"
            (LogMessage Info 29 "la la la")
            (parseMessage "I 29 la la la")
        ),
      TestCase
        ( assertEqual
            "parseMessage This is not in the right format"
            (Unknown "This is not in the right format")
            (parseMessage "This is not in the right format")
        )
    ]
