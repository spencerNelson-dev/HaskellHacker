import Data.List

getConsec :: Int -> [Int] -> [[Int]]
getConsec _ [] = []
getConsec m xs = take m xs : getConsec m (tail xs) 

testString :: [Char]
testString = "5\n1 2 1 3 2\n3 2"

getInput :: String -> [[Int]]
getInput = map(map read . words) . lines

solve :: [[Int]] -> Int 
solve [[l],xs,[m,d]] = length $ filter(\x -> sum x == m) $ filter (\x -> length x == d) $ getConsec d xs    

solve2 ::Int -> Int -> [Int] -> Int 
solve2 m d xs = length $ filter(\x -> sum x == m) $ filter (\x -> length x == d) $ getConsec d xs 
    

main :: IO ()
main = interact $ show . solve . getInput
