module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)

responseFor :: String -> String
responseFor xs
    | all isSpace xsTrimmed        = "Fine. Be that way!"
    | isQuestion && isYelling      = "Calm down, I know what I'm doing!"
    | isYelling                    = "Whoa, chill out!"
    | isQuestion                   = "Sure."
    | otherwise                    = "Whatever."
  where
    xsTrimmed = dropWhile isSpace . reverse . dropWhile isSpace $ reverse xs
    isQuestion = not (null xsTrimmed) && last xsTrimmed == '?'
    hasLetters = any isAlpha xsTrimmed
    isYelling = hasLetters && allUpper xsTrimmed

    allUpper s = all (\c -> not (isAlpha c) || isUpper c) s
