module Main where

import Go
import Text.Read (readMaybe)

-- Read the board interactively from the console.
-- The user types rows one by one; an empty line ends input.
readBoard :: IO [String]
readBoard = do
  putStrLn "Enter board rows (empty line to finish):"
  loop []
  where
    loop acc = do
      line <- getLine
      if null line
         then return (reverse acc)
         else loop (line:acc)

main :: IO ()
main = do
  board <- readBoard

  putStrLn "\nAll territories on this board:"
  let ts = territories board
  mapM_ print ts   -- print each (Owner, [Coords]) pair

  putStrLn "\nCheck a specific coordinate (row col), or blank to quit:"
  coordLoop board

-- Loop asking the user for coordinates to query.
coordLoop :: [String] -> IO ()
coordLoop board = do
  putStr "Coordinate> "
  line <- getLine
  if null line
    then putStrLn "Bye!"
    else case map readMaybe (words line) of
           [Just r, Just c] ->
             case territoryFor board (r,c) of
               Nothing -> putStrLn "Not an empty space!" >> coordLoop board
               Just (owner, coords) -> do
                 putStrLn ("Owner: " ++ show owner)
                 putStrLn ("Coords: " ++ show coords)
                 coordLoop board
           _ -> putStrLn "Enter two numbers!" >> coordLoop board