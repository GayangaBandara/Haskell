-- Main.hs
module Main where

import EnergyPoints (calculateEnergyPoints)

main :: IO ()
main = do
    putStrLn "=== Energy Points Calculator ==="
    putStrLn "Enter the level completed:"
    levelInput <- getLine

    putStrLn "Enter magical item base values separated by spaces (e.g., 3 5):"
    baseInput <- getLine

    let level = read levelInput :: Int
        bases = map read (words baseInput) :: [Int]
        energy = calculateEnergyPoints level bases

    putStrLn $ "Energy points earned: " ++ show energy

    putStrLn "\nPress Enter to exit..."
    _ <- getLine
    return ()
