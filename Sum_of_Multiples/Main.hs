import EnergyPoints (calculateEnergyPoints)

main :: IO ()
main = do
  putStrLn "Welcome to the Energy Points Calculator!"
  putStrLn "Enter the level completed:"
  levelInput <- getLine
  putStrLn "Enter base values of magical items separated by spaces (e.g., 3 5):"
  itemsInput <- getLine

  let level = read levelInput :: Int
      itemBases = map read (words itemsInput) :: [Int]
      points = calculateEnergyPoints level itemBases

  putStrLn $ "You earned " ++ show points ++ " energy points!"
