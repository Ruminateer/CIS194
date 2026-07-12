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
      testCase "-1" ([] @=? toDigitsRev (-1)),
      testCase "large negative" ([] @=? toDigitsRev (-12345))
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
      testCase "-1" ([] @=? toDigits (-1)),
      testCase "large negative" ([] @=? toDigits (-12345))
    ]

testDoubleEveryOther :: TestTree
testDoubleEveryOther =
  testGroup
    "doubleEveryOther"
    [ testCase "[8, 7, 6, 5]" ([16, 7, 12, 5] @=? doubleEveryOther [8, 7, 6, 5]),
      testCase "[1, 2, 3]" ([1, 4, 3] @=? doubleEveryOther [1, 2, 3]),
      testCase "[5, 6]" ([10, 6] @=? doubleEveryOther [5, 6]),
      testCase "four elements" ([2, 2, 6, 4] @=? doubleEveryOther [1, 2, 3, 4]),
      testCase "five elements" ([1, 4, 3, 8, 5] @=? doubleEveryOther [1, 2, 3, 4, 5]),
      testCase "[9]" ([9] @=? doubleEveryOther [9]),
      testCase "[]" ([] @=? doubleEveryOther [])
    ]

testSumDigits :: TestTree
testSumDigits =
  testGroup
    "sumDigits"
    [ testCase "[16, 7, 12, 5]" (22 @=? sumDigits [16, 7, 12, 5]),
      testCase "single digit" (7 @=? sumDigits [7]),
      testCase "two-digit number" (1 @=? sumDigits [10]),
      testCase "two non-zero digits" (18 @=? sumDigits [99]),
      testCase "zeros and two-digit numbers" (6 @=? sumDigits [0, 10, 5]),
      testCase "[]" (0 @=? sumDigits [])
    ]

testValidate :: TestTree
testValidate =
  testGroup
    "validate"
    [ testCase "valid card number" (True @=? validate 4012888888881881),
      testCase "invalid card number" (False @=? validate 4012888888881882),
      testCase "another valid number" (True @=? validate 79927398713),
      testCase "another invalid number" (False @=? validate 79927398714)
    ]

testHanoi :: TestTree
testHanoi =
  testGroup
    "hanoi"
    [ testCase "2 disks" ([("a", "c"), ("a", "b"), ("c", "b")] @=? hanoi 2 "a" "b" "c"),
      testCase
        "3 disks"
        ( [ ("a", "b"),
            ("a", "c"),
            ("b", "c"),
            ("a", "b"),
            ("c", "a"),
            ("c", "b"),
            ("a", "b")
          ]
            @=? hanoi 3 "a" "b" "c"
        ),
      testCase "1 disk" ([("x", "y")] @=? hanoi 1 "x" "y" "z"),
      localOption (mkTimeout 1000000) $
        testCase "negative disks" ([] @=? hanoi (-1) "i" "j" "k"),
      testCase "0 disks" ([] @=? hanoi 0 "i" "j" "k")
    ]

testHanoiAcc :: TestTree
testHanoiAcc =
  testGroup
    "hanoiAcc"
    [ testCase (show n ++ " disks") $
        hanoi n "a" "b" "c" @=? hanoiAcc n "a" "b" "c"
    | n <- [-1 .. 10]
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
      testHanoi,
      testHanoiAcc
    ]
