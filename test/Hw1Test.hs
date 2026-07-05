module Hw1Test where

import Hw1.CreditCardValidator
import Hw1.Hanoi
import Test.Tasty
import Test.Tasty.HUnit

testToDigitsRev :: TestTree
testToDigitsRev =
  testGroup
    "toDigitsRev"
    [ testCase "123" ([3, 2, 1] @=? toDigitsRev 123),
      testCase "8537" ([7, 3, 5, 8] @=? toDigitsRev 8537),
      testCase "0" ([] @=? toDigitsRev 0),
      testCase "1" ([1] @=? toDigitsRev 1),
      testCase "10" ([0, 1] @=? toDigitsRev 10),
      testCase "100" ([0, 0, 1] @=? toDigitsRev 100),
      testCase "105" ([5, 0, 1] @=? toDigitsRev 105),
      testCase "-1" ([] @=? toDigitsRev (-1))
    ]

testToDigits :: TestTree
testToDigits =
  testGroup
    "toDigits"
    [ testCase "123" ([1, 2, 3] @=? toDigits 123),
      testCase "8537" ([8, 5, 3, 7] @=? toDigits 8537),
      testCase "0" ([] @=? toDigits 0),
      testCase "1" ([1] @=? toDigits 1),
      testCase "10" ([1, 0] @=? toDigits 10),
      testCase "100" ([1, 0, 0] @=? toDigits 100),
      testCase "105" ([1, 0, 5] @=? toDigits 105),
      testCase "-1" ([] @=? toDigits (-1))
    ]

testDoubleEveryOther :: TestTree
testDoubleEveryOther =
  testGroup
    "doubleEveryOther"
    [ testCase "[8, 7, 6, 5]" ([16, 7, 12, 5] @=? doubleEveryOther [8, 7, 6, 5]),
      testCase "[1, 2, 3]" ([1, 4, 3] @=? doubleEveryOther [1, 2, 3]),
      testCase "[5, 6]" ([10, 6] @=? doubleEveryOther [5, 6]),
      testCase "[9]" ([9] @=? doubleEveryOther [9]),
      testCase "[]" ([] @=? doubleEveryOther [])
    ]

testSumDigits :: TestTree
testSumDigits =
  testGroup
    "sumDigits"
    [ testCase "[16, 7, 12, 5]" (22 @=? sumDigits [16, 7, 12, 5]),
      testCase "[]" (0 @=? sumDigits [])
    ]

testValidate :: TestTree
testValidate =
  testGroup
    "validate"
    [ testCase "valid card number" (True @=? validate 4012888888881881),
      testCase "invalid card number" (False @=? validate 4012888888881882)
    ]

testHanoi :: TestTree
testHanoi =
  testGroup
    "hanoi"
    [ testCase "2 disks" ([("a", "c"), ("a", "b"), ("c", "b")] @=? hanoi 2 "a" "b" "c"),
      testCase "1 disk" ([("x", "y")] @=? hanoi 1 "x" "y" "z"),
      testCase "0 disks" ([] @=? hanoi 0 "i" "j" "k")
    ]

testAll :: TestTree
testAll =
  testGroup
    "Hw1"
    [ testToDigitsRev,
      testToDigits,
      testDoubleEveryOther,
      testSumDigits,
      testValidate,
      testHanoi
    ]
