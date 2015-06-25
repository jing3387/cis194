module Main where

main :: IO ()
main = undefined

-- Converts a positive integer into a list of digits. Returns the empty list for
-- 0 and negative inputs.
--
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = inner n []
  where inner :: Integer -> [Integer] -> [Integer]
        inner 0 xs = xs
        inner m xs = inner (m `div` 10) ((m `mod` 10):xs)

-- Converts a positive integer into a reverse list of digits. Returns the empty
-- list for 0 and negative inputs.
--
-- Example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- Double every other element of the list beginning from the right, that is,
-- the second-to-last, fourth-to-last, ... numbers are doubled.
--
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  reverse $ inner $ reverse xs
  where inner :: [Integer] -> [Integer]
        inner [] = []
        inner (x:[]) = [x]
        inner (x:y:[]) = x:(2 * y):[]
        inner (x:y:zs) = x:(2 * y):(inner zs)

-- Calculates the sum the sum of the digits of a list of integers. The empty
-- list has a sum of 0.
--
-- Example: sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5 == 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum $ toDigits x) + sumDigits xs

-- Indicates whether an integer could be a valid credit card number.
--
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0
