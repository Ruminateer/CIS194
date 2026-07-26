module Hw6Test (testAll) where

import Control.Exception (evaluate)
import Hw6.Fibonacci
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

testAll :: TestTree
testAll =
  localOption (mkTimeout 5000000) $
    testGroup
      "Hw6"
      [ exercise1Tests,
        exercise2Tests,
        exercise3Tests,
        exercise4Tests,
        exercise5Tests,
        exercise6Tests,
        exercise7Tests
      ]

exercise1Tests :: TestTree
exercise1Tests =
  testGroup
    "Exercise 1: fib and fibs1"
    [ testCase "fib starts with the sequence from the handout" $
        map fib [0 .. 14]
          @?= take 15 expectedFibonacci,
      testCase "fib computes a later value" $
        fib 20 @?= expectedFibonacci !! 20,
      testCase "fibs1 starts with the sequence from the handout" $
        take 15 fibs1
          @?= take 15 expectedFibonacci
    ]

exercise2Tests :: TestTree
exercise2Tests =
  testGroup
    "Exercise 2: fibs2"
    [ testCase "fibs2 agrees with the Fibonacci sequence" $
        take 21 fibs2
          @?= expectedFibonacci,
      localOption (mkTimeout 5000000) $
        testCase "a long prefix can be produced efficiently" $ do
          value <- evaluate (last (take 2000 fibs2))
          assertBool "the 1999th Fibonacci number should be positive" (value > 0)
    ]

exercise3Tests :: TestTree
exercise3Tests =
  testGroup
    "Exercise 3: Stream"
    [ testCase "streamToList exposes an infinite stream as a lazy list" $
        take 6 (streamToList (streamRepeat "hi"))
          @?= replicate 6 "hi",
      localOption (mkTimeout 5000000) $
        testCase "show renders a finite prefix of a stream" $ do
          rendered <- evaluate (show (streamRepeat 'x'))
          renderedLength <- evaluate (length rendered)
          assertBool "the rendered prefix should contain a stream element" ('x' `elem` rendered)
          assertBool "the rendering should not be empty" (renderedLength > 0)
    ]

exercise4Tests :: TestTree
exercise4Tests =
  testGroup
    "Exercise 4: stream utilities"
    [ testCase "streamRepeat repeats its argument" $
        streamPrefix 8 (streamRepeat (3 :: Integer))
          @?= replicate 8 3,
      testCase "streamMap transforms every element" $
        streamPrefix 6 (streamMap (* 3) (streamFromSeed (+ 1) (0 :: Integer)))
          @?= [0, 3, 6, 9, 12, 15],
      testCase "streamMap can change the element type" $
        streamPrefix 5 (streamMap even (streamFromSeed (+ 1) (0 :: Integer)))
          @?= [True, False, True, False, True],
      testCase "streamFromSeed includes the initial seed" $
        streamPrefix 6 (streamFromSeed (* 2) (1 :: Integer))
          @?= [1, 2, 4, 8, 16, 32],
      testCase "streamFromSeed repeatedly applies its unfolding rule" $
        streamPrefix 5 (streamFromSeed reverse "ab")
          @?= ["ab", "ba", "ab", "ba", "ab"]
    ]

exercise5Tests :: TestTree
exercise5Tests =
  testGroup
    "Exercise 5: nats and ruler"
    [ testCase "nats contains the natural numbers starting at zero" $
        streamPrefix 12 nats @?= [0 .. 11],
      testCase "ruler starts with the sequence from the handout" $
        streamPrefix 16 ruler
          @?= [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4],
      testCase "ruler continues correctly across a larger power of two" $
        streamPrefix 32 ruler
          @?= [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5]
    ]

exercise6Tests :: TestTree
exercise6Tests =
  testGroup
    "Exercise 6 (optional): generating functions"
    [ testCase "x has the coefficient stream 0, 1, 0, ..." $
        streamPrefix 7 x @?= [0, 1, 0, 0, 0, 0, 0],
      testCase "fromInteger creates a constant generating function" $
        streamPrefix 6 (7 :: Stream Integer) @?= [7, 0, 0, 0, 0, 0],
      testCase "negate negates every coefficient" $
        streamPrefix 6 (negate (1 + x + x ^ (3 :: Integer)))
          @?= [-1, -1, 0, -1, 0, 0],
      testCase "addition adds corresponding coefficients" $
        streamPrefix 6 ((3 + x) + (2 + x ^ (2 :: Integer)))
          @?= [5, 1, 1, 0, 0, 0],
      testCase "multiplication performs coefficient convolution" $
        streamPrefix 7 ((1 + x) ^ (5 :: Integer))
          @?= [1, 5, 10, 10, 5, 1, 0],
      testCase "multiplication handles the polynomial example" $
        streamPrefix 6 ((x ^ (2 :: Integer) + x + 3) * (x - 5))
          @?= [-15, -2, -4, 1, 0, 0],
      testCase "division produces the geometric-series coefficients" $
        streamPrefix 8 (1 / (1 - x))
          @?= replicate 8 1,
      testCase "fibs3 produces Fibonacci coefficients" $
        streamPrefix 16 fibs3
          @?= take 16 expectedFibonacci
    ]

exercise7Tests :: TestTree
exercise7Tests =
  testGroup
    "Exercise 7 (optional): matrix Fibonacci"
    [ testCase "fib4 handles zero" $
        fib4 0 @?= 0,
      testCase "fib4 handles the first two nonzero indices" $ do
        fib4 1 @?= 1
        fib4 2 @?= 1,
      testCase "fib4 agrees with the handout sequence" $
        map fib4 [0 .. 14]
          @?= take 15 expectedFibonacci,
      testCase "fib4 computes a much later value" $
        fib4 100 @?= 354224848179261915075
    ]

streamPrefix :: Int -> Stream a -> [a]
streamPrefix count = take count . streamToList

expectedFibonacci :: [Integer]
expectedFibonacci =
  [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]
