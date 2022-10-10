import System.Environment

main = do
    putStrLn "Olá mundo!"
    name <- getLine
    args <- getArgs
    putStrLn name
    putStrLn $ head args