module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

-- | Calculates the sum of all unique multiples of given factors below a given limit.
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    sum . nub . concatMap multiplesBelow $ factors
  where
    multiplesBelow :: Integer -> [Integer]
    multiplesBelow n
        | n <= 0    = []  -- ignore zero or negative factors
        | otherwise = [x | x <- [n, 2*n .. limit - 1]]
