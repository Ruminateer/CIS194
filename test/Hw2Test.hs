module Hw2Test where

import Hw2.Log hiding (testParse, testWhatWentWrong)
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
        "warning"
        ( (LogMessage Warning 5 "Flange is due for a check-up")
            @=? parseMessage "W 5 Flange is due for a check-up"
        ),
      testCase
        "unknown"
        ( (Unknown "This is not in the right format")
            @=? parseMessage "This is not in the right format"
        ),
      testCase
        "empty line"
        ( Unknown ""
            @=? parseMessage ""
        ),
      testCase
        "missing error timestamp"
        ( Unknown "E 2"
            @=? parseMessage "E 2"
        ),
      testCase
        "non-numeric timestamp"
        ( Unknown "I soon la la la"
            @=? parseMessage "I soon la la la"
        ),
      testCase
        "non-numeric error severity"
        ( Unknown "E severe 562 help help"
            @=? parseMessage "E severe 562 help help"
        )
    ]

testParse :: TestTree
testParse =
  testGroup
    "parse"
    [ testCase "empty input" ([] @=? parse ""),
      testCase
        "multiple lines"
        ( [ LogMessage Info 2 "second",
            LogMessage Warning 1 "first",
            Unknown "not a log message"
          ]
            @=? parse "I 2 second\nW 1 first\nnot a log message\n"
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
        ),
      testCase
        "earlier timestamp goes left"
        ( Node
            (Node Leaf (LogMessage Info 3 "time=3") Leaf)
            (LogMessage Info 5 "time=5")
            Leaf
            @=? insert
              (LogMessage Info 3 "time=3")
              (Node Leaf (LogMessage Info 5 "time=5") Leaf)
        )
    ]

testBuild :: TestTree
testBuild =
  testGroup
    "build"
    [ testCase "empty list" (Leaf @=? build []),
      testCase
        "successive insertion ignores unknown messages"
        ( [ LogMessage Warning 1 "first",
            LogMessage Info 2 "second",
            LogMessage (Error 70) 3 "third"
          ]
            @=? inOrder
              ( build
                  [ LogMessage Info 2 "second",
                    Unknown "garbage",
                    LogMessage (Error 70) 3 "third",
                    LogMessage Warning 1 "first"
                  ]
              )
        )
    ]

testInOrder :: TestTree
testInOrder =
  testGroup
    "inOrder"
    [ testCase "leaf" ([] @=? inOrder Leaf),
      testCase
        "sample tree"
        ( [ LogMessage Warning 2 "t=2",
            LogMessage Warning 3 "t=3",
            LogMessage Info 5 "t=5",
            LogMessage (Error 0) 8 "t=8",
            LogMessage (Error 0) 11 "t=11"
          ]
            @=? inOrder sampleTree
        )
    ]

testWhatWentWrong :: TestTree
testWhatWentWrong =
  testGroup
    "whatWentWrong"
    [ testCase "empty list" ([] @=? whatWentWrong []),
      testCase
        "filters, includes severity 50, and sorts by timestamp"
        ( ["exactly fifty", "later failure"]
            @=? whatWentWrong
              [ LogMessage (Error 80) 9 "later failure",
                LogMessage Info 1 "information",
                LogMessage (Error 49) 2 "not severe enough",
                Unknown "garbage",
                LogMessage (Error 50) 4 "exactly fifty",
                LogMessage Warning 3 "warning"
              ]
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
      testParse,
      testInsert,
      testBuild,
      testInOrder,
      testWhatWentWrong,
      testSampleLog
    ]
