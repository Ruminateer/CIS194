import Test.HUnit

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

testToDigitsRev :: Test
testToDigitsRev =
  TestList
    [ TestCase (assertEqual "toDigitsRev 123" [3, 2, 1] (toDigitsRev 123)),
      TestCase (assertEqual "toDigitsRev 8537" [7, 3, 5, 8] (toDigitsRev 8537)),
      TestCase (assertEqual "toDigitsRev 0" [] (toDigitsRev 0)),
      TestCase (assertEqual "toDigitsRev 1" [1] (toDigitsRev 1)),
      TestCase (assertEqual "toDigitsRev (-1)" [] (toDigitsRev (-1)))
    ]

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

testToDigits :: Test
testToDigits =
  TestList
    [ TestCase (assertEqual "toDigits 123" [1, 2, 3] (toDigits 123)),
      TestCase (assertEqual "toDigits 8537" [8, 5, 3, 7] (toDigits 8537)),
      TestCase (assertEqual "toDigits 0" [] (toDigits 0)),
      TestCase (assertEqual "toDigits 1" [1] (toDigits 1)),
      TestCase (assertEqual "toDigits (-1)" [] (toDigits (-1)))
    ]

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (x : y : zs) = x : y + y : doubleEveryOtherLeft zs
doubleEveryOtherLeft l = l

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeft . reverse

testDoubleEveryOther :: Test
testDoubleEveryOther =
  TestList
    [ TestCase (assertEqual "doubleEveryOther [8, 7, 6, 5]" [16, 7, 12, 5] (doubleEveryOther [8, 7, 6, 5])),
      TestCase (assertEqual "doubleEveryOther [1, 2, 3]" [1, 4, 3] (doubleEveryOther [1, 2, 3])),
      TestCase (assertEqual "doubleEveryOther [5, 6]" [10, 6] (doubleEveryOther [5, 6])),
      TestCase (assertEqual "doubleEveryOther [9]" [9] (doubleEveryOther [9])),
      TestCase (assertEqual "doubleEveryOther []" [] (doubleEveryOther []))
    ]

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigitsRev)

testSumDigits :: Test
testSumDigits =
  TestList
    [ TestCase (assertEqual "sumDigits [16, 7, 12, 5]" 22 (sumDigits [16, 7, 12, 5])),
      TestCase (assertEqual "sumDigits []" 0 (sumDigits []))
    ]

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOtherLeft (toDigitsRev n))) 10 == 0

testValidate :: Test
testValidate =
  TestList
    [ TestCase (assertBool "validate 4012888888881881" (validate 4012888888881881)),
      TestCase (assertBool "not (validate 4012888888881882)" (not (validate 4012888888881882)))
    ]

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

testHanoi :: Test
testHanoi =
  TestList
    [ TestCase (assertEqual "hanoi 2 a b c" [("a", "c"), ("a", "b"), ("c", "b")] (hanoi 2 "a" "b" "c")),
      TestCase (assertEqual "hanoi 1 x y z" [("x", "y")] (hanoi 1 "x" "y" "z")),
      TestCase (assertEqual "hanoi 0 i j k" [] (hanoi 0 "i" "j" "k"))
    ]
