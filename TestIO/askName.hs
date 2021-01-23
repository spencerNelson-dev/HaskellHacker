module Main where


main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine

    putStrLn $ greeting name

greeting :: String -> String 
greeting name = "Hello " ++ name ++ ", it is nice to meet you!"