module Main where

import SpaceAge

main :: IO ()
main = do
  putStrLn "Enter age in seconds:"
  input <- getLine
  let seconds = read input :: Float
  putStrLn $ "Age on Earth: " ++ show (ageOn Earth seconds)

  -- Keep window open
  _ <- getLine
  return ()
