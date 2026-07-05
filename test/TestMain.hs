module Main (main) where

import Hw1Test
import Hw2Test
import Hw3Test
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "CIS194"
      [ Hw1Test.testAll,
        Hw2Test.testAll,
        Hw3Test.testAll
      ]
