module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid input
    | length digits <= 1 = False
    | not (all isDigit digits) = False
    | otherwise = total `mod` 10 == 0
  where
    digits = filter (/= ' ') input
    reversed = reverse digits
    transformed = zipWith luhnDouble [0..] reversed
    total = sum transformed

luhnDouble :: Int -> Char -> Int
luhnDouble idx ch
    | even idx  = digitToInt ch
    | otherwise =
        let d = digitToInt ch * 2
        in if d > 9 then d - 9 else d
