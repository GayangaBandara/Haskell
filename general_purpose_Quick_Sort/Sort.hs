module Sort (quickSort) where

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []  
quickSort cmp (x:xs) =
    let left  = quickSort cmp [a | a <- xs, cmp a x]
        right = quickSort cmp [a | a <- xs, not (cmp a x)]
    in  left ++ [x] ++ right
