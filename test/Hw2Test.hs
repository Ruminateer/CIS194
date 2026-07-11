module Hw2Test where

import Hw2.Log
import Hw2.LogAnalysis
import Paths_CIS194 (getDataFileName)
import Test.Tasty
import Test.Tasty.HUnit

testParseMessage :: TestTree
testParseMessage =
  testGroup
    "parseMessage"
    [ testCase
        "error"
        ( (LogMessage (Error 2) 562 "help help")
            @=? parseMessage "E 2 562 help help"
        ),
      testCase
        "info"
        ( (LogMessage Info 29 "la la la")
            @=? parseMessage "I 29 la la la"
        ),
      testCase
        "unknown"
        ( (Unknown "This is not in the right format")
            @=? parseMessage "This is not in the right format"
        )
    ]

sampleTree :: MessageTree
sampleTree =
  Node
    ( Node
        (Node Leaf (LogMessage Warning 2 "t=2") Leaf)
        (LogMessage Warning 3 "t=3")
        Leaf
    )
    (LogMessage Info 5 "t=5")
    ( Node
        Leaf
        (LogMessage (Error 0) 8 "t=8")
        (Node Leaf (LogMessage (Error 0) 11 "t=11") Leaf)
    )

testInsert :: TestTree
testInsert =
  testGroup
    "insert"
    [ testCase
        "Unknown to leaf"
        ( Leaf
            @=? insert (Unknown "garbage msg") Leaf
        ),
      testCase
        "Unknown to sampleTree"
        ( sampleTree
            @=? insert (Unknown "garbage msg") sampleTree
        ),
      testCase
        "time=6 to leaf"
        ( (Node Leaf (LogMessage Info 6 "time=6") Leaf)
            @=? insert (LogMessage Info 6 "time=6") Leaf
        ),
      testCase
        "time=6 to sampleTree"
        ( ( Node
              ( Node
                  (Node Leaf (LogMessage Warning 2 "t=2") Leaf)
                  (LogMessage Warning 3 "t=3")
                  Leaf
              )
              (LogMessage Info 5 "t=5")
              ( Node
                  (Node Leaf (LogMessage Info 6 "time=6") Leaf)
                  (LogMessage (Error 0) 8 "t=8")
                  (Node Leaf (LogMessage (Error 0) 11 "t=11") Leaf)
              )
          )
            @=? insert (LogMessage Info 6 "time=6") sampleTree
        )
    ]

testSampleLog :: TestTree
testSampleLog =
  testCase "sample.log" $ do
    sampleLogPath <- getDataFileName "test/data/Hw2/sample.log"
    messages <- parse <$> readFile sampleLogPath
    [ "Way too many pickles",
      "Bad pickle-flange interaction detected",
      "Flange failed!"
      ]
      @=? whatWentWrong messages

testAll :: TestTree
testAll =
  testGroup
    "Hw2"
    [ testParseMessage,
      testInsert,
      testSampleLog
    ]
