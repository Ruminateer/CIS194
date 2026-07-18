module Hw3Test where

import Hw3.Golf (histogram, localMaxima, skips)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

testSkips :: TestTree
testSkips =
  testGroup
    "skips"
    [ testCase "empty input has empty output" $
        [] @=? (skips [] :: [[Int]]),
      testCase "singleton input" $
        [[1]] @=? skips [1 :: Int],
      testCase "two elements" $
        [[True, False], [False]] @=? skips [True, False],
      testCase "handout alphabet example" $
        ["ABCD", "BD", "C", "D"] @=? skips "ABCD",
      testCase "handout hello example" $
        ["hello!", "el!", "l!", "l", "o", "!"] @=? skips "hello!",
      testCase "odd-length input" $
        [ [10, 20, 30, 40, 50],
          [20, 40],
          [30],
          [40],
          [50]
        ]
          @=? skips [10, 20, 30, 40, 50 :: Int],
      testCase "repeated values retain their positions" $
        [ [1, 1, 2, 1, 2, 2],
          [1, 1, 2],
          [2, 2],
          [1],
          [2],
          [2]
        ]
          @=? skips [1, 1, 2, 1, 2, 2 :: Int]
    ]

testLocalMaxima :: TestTree
testLocalMaxima =
  testGroup
    "localMaxima"
    [ testCase "empty list" $
        [] @=? localMaxima [],
      testCase "one element cannot be a local maximum" $
        [] @=? localMaxima [9],
      testCase "two elements cannot contain a local maximum" $
        [] @=? localMaxima [9, 1],
      testCase "first and last elements are never local maxima" $
        [2] @=? localMaxima [9, 1, 2, 1, 9],
      testCase "single three-element peak" $
        [2] @=? localMaxima [1, 2, 1],
      testCase "handout multiple-peak example" $
        [9, 6] @=? localMaxima [2, 9, 5, 6, 1],
      testCase "handout middle-peak example" $
        [4] @=? localMaxima [2, 3, 4, 1, 5],
      testCase "strictly increasing list" $
        [] @=? localMaxima [1, 2, 3, 4, 5],
      testCase "strictly decreasing list" $
        [] @=? localMaxima [5, 4, 3, 2, 1],
      testCase "equal neighbors disqualify a candidate" $
        [] @=? localMaxima [1, 2, 2, 1],
      testCase "one equal neighbor is enough to disqualify" $
        [] @=? localMaxima [1, 3, 3, 2, 2, 1],
      testCase "negative values and peaks are returned in order" $
        [-2, 0, -1] @=? localMaxima [-3, -2, -4, 0, -5, -1, -2]
    ]

testHistogram :: TestTree
testHistogram =
  testGroup
    "histogram"
    [ testCase "empty input contains only the axes" $
        "==========\n0123456789\n" @=? histogram [],
      testCase "one occurrence in the first column" $
        "*         \n==========\n0123456789\n" @=? histogram [0],
      testCase "one occurrence in the last column" $
        "         *\n==========\n0123456789\n" @=? histogram [9],
      testCase "one occurrence of every digit" $
        "**********\n==========\n0123456789\n"
          @=? histogram [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
      testCase "handout first example" $
        concat
          [ " *        \n",
            " *        \n",
            " *   *    \n",
            "==========\n",
            "0123456789\n"
          ]
          @=? histogram [1, 1, 1, 5],
      testCase "handout second example" $
        concat
          [ "    *     \n",
            "    *     \n",
            "    * *   \n",
            " ******  *\n",
            "==========\n",
            "0123456789\n"
          ]
          @=? histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9],
      testCase "rows are printed from highest level to lowest" $
        concat
          [ "*        *\n",
            "* *      *\n",
            "* *   *  *\n",
            "==========\n",
            "0123456789\n"
          ]
          @=? histogram [0, 0, 0, 2, 2, 6, 9, 9, 9]
    ]

testAll :: TestTree
testAll =
  testGroup
    "Hw3"
    [ testSkips,
      testLocalMaxima,
      testHistogram
    ]
