-- Main.hs
import Luhn (isValid)

main :: IO ()
main = do
    putStrLn "Enter a credit card number (with or without spaces):"
    input <- getLine
    if isValid input
        then putStrLn "This number is valid according to the Luhn algorithm."
        else putStrLn "This number is NOT valid."
