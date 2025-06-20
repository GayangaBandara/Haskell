module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text =
    let lowerText = map toLower text -- Convert all characters to lowercase
        onlyLetters = filter (`elem` ['a'..'z']) lowerText --Keep only letters a-z
        uniqueLetters = nub onlyLetters --Remove Duplicate letters
    in length uniqueLetters == 26 -- Check if we have 26 different letters