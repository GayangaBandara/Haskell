isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | otherwise = null [ x | x <- [2..isqrt n], n `mod` x == 0 ]

-- Helper function to compute integer square root
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
