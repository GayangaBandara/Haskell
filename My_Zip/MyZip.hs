module MyZip (myZip) where

myZip :: [a] -> [b] -> [(a, b)]
myZip = zipWith (,)
