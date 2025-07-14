slice_array :: Int -> [a] -> [a]
slice_array 0 _      = []
slice_array _ []     = []
slice_array n (x:xs) = x : slice_array (n - 1) xs