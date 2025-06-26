import Collatz (collatzSteps)

main :: IO ()
main = do
    putStrLn "Enter a positive integer:"
    input <- getLine
    let n = read input :: Integer
    if n <= 0
        then putStrLn "Please enter a positive integer greater than 0."
        else putStrLn ("Number of steps to reach 1: " ++ show (collatzSteps n))
