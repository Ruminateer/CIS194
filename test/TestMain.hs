module Main (main) where

import Hw1Test
import Hw2Test
import Test.HUnit

main :: IO Counts
main =
  runTestTT $
    TestList
      [ Hw1Test.testAll,
        Hw2Test.testAll
      ]
