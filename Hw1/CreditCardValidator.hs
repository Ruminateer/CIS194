module Hw1.CreditCardValidator where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (x : y : zs) = x : y + y : doubleEveryOtherLeft zs
doubleEveryOtherLeft l = l

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigitsRev)

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOtherLeft (toDigitsRev n))) 10 == 0
