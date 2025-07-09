module Hw2Test where

import Hw2.Log
import Hw2.LogAnalysis
import Test.HUnit

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

testAll :: Test
testAll =
  TestList
    [ testParseMessage
    ]
