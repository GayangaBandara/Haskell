module Main where

import LeapYear (isLeapYear)

main :: IO ()
main = do
    putStrLn "============================"
    putStrLn "      Leap Year Checker    "
    putStrLn "============================"
    putStrLn ""
    putStrLn "Enter a year:"
    input <- getLine
    let year = read input :: Int
    if isLeapYear year
        then putStrLn (show year ++ " is a leap year.")
        else putStrLn (show year ++ " is not a leap year.")

    _ <- getLine
    return ()