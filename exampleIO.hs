
main = do  
    putStrLn "Please Enter Your Name: "
    name <- getLine
    putStrLn ("Hello" ++ name)