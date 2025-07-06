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