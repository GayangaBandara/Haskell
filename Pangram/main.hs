import Pangram (isPangram)
import System.IO (hFlush, stdout)
import Data.Char (toLower)

main :: IO ()
main = do
    putStrLn "==============================="
    putStrLn "      Pangram Checker App      "
    putStrLn "===============================\n"
    runApp

runApp :: IO ()
runApp = do
    putStrLn "Enter a sentence to check if it's a pangram:"
    putStr "> "
    hFlush stdout
    sentence <- getLine

    if isPangram sentence
        then putStrLn "\nResult: This sentence is a pangram (contains all 26 letters)."
        else putStrLn "\nResult: This sentence is NOT a pangram."

    putStrLn "\nDo you want to check another sentence? (y/n)"
    putStr "> "
    hFlush stdout
    answer <- getLine

    if map toLower answer == "y"
        then do
            putStrLn ""
            runApp
        else putStrLn "\nThank you for using the Pangram Checker. Goodbye!"

    _ <- getLine
    return ()
