module Main (main) where

import Hw1Test
import Hw2Test
import Hw3Test
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

main :: IO ()
main =
  defaultMain $
    hUnitTestToTests $
      TestList
        [ TestLabel "Hw1Test" Hw1Test.testAll,
          TestLabel "Hw2Test" Hw2Test.testAll,
          TestLabel "Hw3Test" Hw3Test.testAll
        ]
