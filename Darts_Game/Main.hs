module Main where
import Darts (score)
import Text.Printf (printf)

main :: IO ()
main = do
    putStrLn "============================"
    putStrLn "        Darts Scorer        "
    putStrLn "============================"
    putStrLn ""
    putStrLn "Enter dart coordinates (x and y):"
    putStr   "  x = "
    xStr <- getLine
    putStr   "  y = "
    yStr <- getLine

    let x = read xStr :: Float
        y = read yStr :: Float
        result = score x y

    putStrLn ""
    printf "Dart landed at (%.2f, %.2f)\n" x y
    putStrLn $ "You scored: " ++ show result ++ " point(s)"
    putStrLn ""
    
    _ <- getLine
    return ()