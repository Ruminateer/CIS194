module Hw3Test where

import Hw3.Golf
import Test.Tasty
import Test.Tasty.HUnit

testSkips :: TestTree
testSkips =
  testGroup
    "skips"
    [ testCase "ABCD"
        ( assertEqual
            "skips ABCD"
            ["ABCD", "BD", "C", "D"]
            (skips "ABCD")
        ),
      testCase "hello!"
        ( assertEqual
            "skips hello!"
            ["hello!", "el!", "l!", "l", "o", "!"]
            (skips "hello!")
        ),
      testCase "[1]"
        ( assertEqual
            "skips [1]"
            [[1]]
            (skips [1 :: Int])
        ),
      testCase "[True, False]"
        ( assertEqual
            "skips [True, False]"
            [[True, False], [False]]
            (skips [True, False])
        ),
      testCase "[]"
        ( assertEqual
            "skips []"
            []
            (skips [] :: [[Int]])
        )
    ]

testLocalMaxima :: TestTree
testLocalMaxima =
  testGroup
    "localMaxima"
    [ testCase "[2, 9, 5, 6, 1]"
        ( assertEqual
            "localMaxima [2, 9, 5, 6, 1]"
            [9, 6]
            (localMaxima [2, 9, 5, 6, 1])
        ),
      testCase "[2, 3, 4, 1, 5]"
        ( assertEqual
            "localMaxima [2, 3, 4, 1, 5]"
            [4]
            (localMaxima [2, 3, 4, 1, 5])
        ),
      testCase "[1, 2, 3, 4, 5]"
        ( assertEqual
            "localMaxima [1, 2, 3, 4, 5]"
            []
            (localMaxima [1, 2, 3, 4, 5])
        )
    ]

testAll :: TestTree
testAll =
  testGroup
    "Hw3"
    [ testSkips,
      testLocalMaxima
    ]
