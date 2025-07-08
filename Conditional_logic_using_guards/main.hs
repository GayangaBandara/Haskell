f :: Int -> Int
f x
  | x < 0     = x * (-1)
  | x < 10    = x
  | x > 11    = 2 * x
  | True      = 19

main :: IO ()
main = loop

loop :: IO ()
loop = do
    putStrLn "\nEnter a number:"
    input <- getLine
    let number = read input :: Int
    let result = f number
    putStrLn ("f " ++ show number ++ " = " ++ show result)
    loop  -- call loop again for next input
