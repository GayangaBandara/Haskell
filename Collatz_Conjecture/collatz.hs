module Collatz (collatzSteps) where

collatzSteps :: Integer -> Integer
collatzSteps n
    | n <= 0    = error "Input must be a positive integer"
    | otherwise = fromIntegral (steps n)
  where
    steps 1 = 0
    steps x
        | even x    = 1 + steps (x `div` 2)
        | otherwise = 1 + steps (3 * x + 1)
