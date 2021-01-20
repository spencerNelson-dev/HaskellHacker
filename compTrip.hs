testInput :: [Int]
testInput = [5, 6, 7, 3, 6, 10]

cutInHalf :: [Int] -> [[Int]]
cutInHalf xs = [take mid xs, drop mid xs]
    where mid = length xs `div` 2

compareLists :: [[Int]] -> [Int]
compareLists [as, bs] =
    [aPoints, bPoints]
    where 
        tally = zipWith (-) as bs
        aPoints = length $ filter (> 0) tally
        bPoints = length $ filter (< 0) tally

solve :: [Int] -> [Int]
solve =  compareLists . cutInHalf

main :: IO ()
main = interact $ unwords . map show . solve . map read . words