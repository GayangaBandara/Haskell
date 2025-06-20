module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- Normalize a string: lowercase and sorted letters
normalize :: String -> String
normalize = sort . map toLower

-- Check if two words are anagrams (excluding identical words)
isAnagram :: String -> String -> Bool
isAnagram target candidate =
  let normTarget = normalize target
      normCandidate = normalize candidate
  in normTarget == normCandidate &&
     map toLower target /= map toLower candidate

-- Return all valid anagrams of the target from the candidate list
anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter (isAnagram target) candidates
