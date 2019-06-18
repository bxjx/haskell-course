module Exercise1
    ( validate
    ) where

import Data.Char

toDigits :: Int -> [Int]
toDigits n
  | n < 1 = []
  | otherwise = digitToInt <$> show n

-- toDigitsRev :: Int -> [Int]
-- toDigitsRev = reverse . toDigits

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

sumDigits :: [Int] -> Int
sumDigits = sum . fmap ( sum . toDigits )

validate :: Int -> Bool
-- validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0
