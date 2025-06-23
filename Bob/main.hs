-- main.hs
import Bob (responseFor)
import System.IO (hFlush, stdout)
import Data.Char (toLower)  

main :: IO ()
main = do
    putStrLn "==============================="
    putStrLn "         Talk to Bob           "
    putStrLn "===============================\n"
    chatLoop

chatLoop :: IO ()
chatLoop = do
    putStr "You: "
    hFlush stdout
    input <- getLine
    let reply = responseFor input
    putStrLn ("Bob: " ++ reply)

    putStrLn "\nDo you want to say something else to Bob? (y/n)"
    putStr "> "
    hFlush stdout
    answer <- getLine
    if map toLower answer == "y"
        then do
            putStrLn ""
            chatLoop
        else putStrLn "\nBob: Later."
