module Main where


main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    contents <- readFile "input.txt"
    writeFile "output.txt" name

    outputFile <- readFile "output.txt"

    putStrLn $ greeting name

    putStrLn ("output.txt reads:" ++ outputFile)

greeting :: String -> String 
greeting name = "Hello " ++ name ++ ", it is nice to meet you!"