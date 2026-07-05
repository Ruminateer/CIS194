module Hw1Test where

import Hw1.CreditCardValidator
import Hw1.Hanoi
import Test.Tasty
import Test.Tasty.HUnit

testToDigitsRev :: TestTree
testToDigitsRev =
  testGroup
    "toDigitsRev"
    [ testCase "123" (assertEqual "toDigitsRev 123" [3, 2, 1] (toDigitsRev 123)),
      testCase "8537" (assertEqual "toDigitsRev 8537" [7, 3, 5, 8] (toDigitsRev 8537)),
      testCase "0" (assertEqual "toDigitsRev 0" [] (toDigitsRev 0)),
      testCase "1" (assertEqual "toDigitsRev 1" [1] (toDigitsRev 1)),
      testCase "-1" (assertEqual "toDigitsRev (-1)" [] (toDigitsRev (-1)))
    ]

testToDigits :: TestTree
testToDigits =
  testGroup
    "toDigits"
    [ testCase "123" (assertEqual "toDigits 123" [1, 2, 3] (toDigits 123)),
      testCase "8537" (assertEqual "toDigits 8537" [8, 5, 3, 7] (toDigits 8537)),
      testCase "0" (assertEqual "toDigits 0" [] (toDigits 0)),
      testCase "1" (assertEqual "toDigits 1" [1] (toDigits 1)),
      testCase "-1" (assertEqual "toDigits (-1)" [] (toDigits (-1)))
    ]

testDoubleEveryOther :: TestTree
testDoubleEveryOther =
  testGroup
    "doubleEveryOther"
    [ testCase "[8, 7, 6, 5]" (assertEqual "doubleEveryOther [8, 7, 6, 5]" [16, 7, 12, 5] (doubleEveryOther [8, 7, 6, 5])),
      testCase "[1, 2, 3]" (assertEqual "doubleEveryOther [1, 2, 3]" [1, 4, 3] (doubleEveryOther [1, 2, 3])),
      testCase "[5, 6]" (assertEqual "doubleEveryOther [5, 6]" [10, 6] (doubleEveryOther [5, 6])),
      testCase "[9]" (assertEqual "doubleEveryOther [9]" [9] (doubleEveryOther [9])),
      testCase "[]" (assertEqual "doubleEveryOther []" [] (doubleEveryOther []))
    ]

testSumDigits :: TestTree
testSumDigits =
  testGroup
    "sumDigits"
    [ testCase "[16, 7, 12, 5]" (assertEqual "sumDigits [16, 7, 12, 5]" 22 (sumDigits [16, 7, 12, 5])),
      testCase "[]" (assertEqual "sumDigits []" 0 (sumDigits []))
    ]

testValidate :: TestTree
testValidate =
  testGroup
    "validate"
    [ testCase "valid card number" (assertBool "validate 4012888888881881" (validate 4012888888881881)),
      testCase "invalid card number" (assertBool "not (validate 4012888888881882)" (not (validate 4012888888881882)))
    ]

testHanoi :: TestTree
testHanoi =
  testGroup
    "hanoi"
    [ testCase "2 disks" (assertEqual "hanoi 2 a b c" [("a", "c"), ("a", "b"), ("c", "b")] (hanoi 2 "a" "b" "c")),
      testCase "1 disk" (assertEqual "hanoi 1 x y z" [("x", "y")] (hanoi 1 "x" "y" "z")),
      testCase "0 disks" (assertEqual "hanoi 0 i j k" [] (hanoi 0 "i" "j" "k"))
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
