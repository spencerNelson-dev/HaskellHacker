-- Between two sets

solve :: [Int] -> [Int] -> Int
solve as bs = length 
                $ filter (\x -> bsGcd `mod` x == 0) 
                $ takeWhile (<= bsGcd) 
                $ map ( * asLcm) [1..]
    where   asLcm = fold lcm as
            bsGcd = fold gcd bs

readIntList :: IO [Int]
readIntList = do map read . words <$> getLine
-- same as line <- getLine
-- return $ map read $ words line

main :: IO ()
main = do   [n, m]  <- readIntList
            as      <- readIntList
            bs      <- readIntList
            print( solve as bs)

-- our own foldr
fold :: (a -> a -> a) -> [a] -> a 
fold f [] = error "list empty"
fold f [x] = x
fold f (x:xs) = f x (fold f xs)          