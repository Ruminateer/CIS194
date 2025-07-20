module Hw3Test where

import Hw3.Golf
import Test.HUnit

testSkips :: Test
testSkips =
  TestList
    [ TestCase
        ( assertEqual
            "skips ABCD"
            ["ABCD", "BD", "C", "D"]
            (skips "ABCD")
        ),
      TestCase
        ( assertEqual
            "skips hello!"
            ["hello!", "el!", "l!", "l", "o", "!"]
            (skips "hello!")
        ),
      TestCase
        ( assertEqual
            "skips [1]"
            [[1]]
            (skips [1 :: Int])
        ),
      TestCase
        ( assertEqual
            "skips [True, False]"
            [[True, False], [False]]
            (skips [True, False])
        ),
      TestCase
        ( assertEqual
            "skips []"
            []
            (skips [] :: [[Int]])
        )
    ]

testLocalMaxima :: Test
testLocalMaxima =
  TestList
    [ TestCase
        ( assertEqual
            "localMaxima [2, 9, 5, 6, 1]"
            [9, 6]
            (localMaxima [2, 9, 5, 6, 1])
        ),
      TestCase
        ( assertEqual
            "localMaxima [2, 3, 4, 1, 5]"
            [4]
            (localMaxima [2, 3, 4, 1, 5])
        ),
      TestCase
        ( assertEqual
            "localMaxima [1, 2, 3, 4, 5]"
            []
            (localMaxima [1, 2, 3, 4, 5])
        )
    ]

testAll :: Test
testAll =
  TestList
    [ TestLabel "test skips" testSkips,
      TestLabel "test localMaxima" testLocalMaxima
    ]
