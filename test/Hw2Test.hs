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

testInsert :: Test
testInsert =
  TestList
    [ TestCase
        ( assertEqual
            "insert Unknown to leaf"
            Leaf
            (insert (Unknown "garbage msg") Leaf)
        ),
      TestCase
        ( assertEqual
            "insert Unknown to sampleTree"
            sampleTree
            (insert (Unknown "garbage msg") sampleTree)
        ),
      TestCase
        ( assertEqual
            "insert time=6 to leaf"
            (Node Leaf (LogMessage Info 6 "time=6") Leaf)
            (insert (LogMessage Info 6 "time=6") Leaf)
        ),
      TestCase
        ( assertEqual
            "insert time=6 to sampleTree"
            ( Node
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
            (insert (LogMessage Info 6 "time=6") sampleTree)
        )
    ]

testSampleLog :: Test
testSampleLog =
  TestCase
    -- TODO: how to use external file for testing?
    ( assertEqual
        "sample.log"
        [ "Way too many pickles",
          "Bad pickle-flange interaction detected",
          "Flange failed!"
        ]
        ( whatWentWrong
            ( parse
                "I 6 Completed armadillo processing\n\
                \I 1 Nothing to report\n\
                \I 4 Everything normal\n\
                \I 11 Initiating self-destruct sequence\n\
                \E 70 3 Way too many pickles\n\
                \E 65 8 Bad pickle-flange interaction detected\n\
                \W 5 Flange is due for a check-up\n\
                \I 7 Out for lunch, back in two time steps\n\
                \E 20 2 Too many pickles\n\
                \I 9 Back from lunch\n\
                \E 99 10 Flange failed!"
            )
        )
    )

testAll :: Test
testAll =
  TestList
    [ TestLabel "test parseMessage" testParseMessage,
      TestLabel "test insert" testInsert,
      TestLabel "test sample.log" testSampleLog
    ]
