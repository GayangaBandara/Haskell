module Series (series) where

series :: [Integer]
series = s
  where
    s = [1, 2, 2] ++ zipWith3 (\a b c -> a + b + c) s (drop 1 s) (drop 2 s)
