-- Main.hs

module Main where

import Anagram (anagramsFor)
import Data.List (intercalate)

main :: IO ()
main = do
  putStrLn "------------------------------"
  putStrLn "        Anagram Finder        "
  putStrLn "------------------------------"

  putStrLn ""
  putStrLn "Enter the target word:"
  target <- getLine

  putStrLn "Enter candidate words (separated by spaces):"
  line <- getLine
  let candidates = words line
      anagrams = anagramsFor target candidates

  putStrLn ""
  putStrLn "Anagrams found:"
  if null anagrams
    then putStrLn "  None found."
    else putStrLn $ "  " ++ intercalate ", " anagrams

  putStrLn ""
  _ <- getLine
  return ()
