import Data.List

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

solve :: [Int] -> Int 
solve (m:xs) =  length . filter(\[x,y] -> (x + y) `mod` m == 0 ) . filter(\x -> length x == 2) $ subsequences xs

fastPairs :: [Int] -> [[Int]]
fastPairs [] = []
fastPairs (x:xs) = map (\y -> [x,y]) xs ++ fastPairs xs

fastSolve :: [Int] -> Int 
fastSolve (m:xs) =  length . filter(\[x,y] -> (x + y) `mod` m == 0 ) $ fastPairs xs

main :: IO ()
main = interact $ show . fastSolve . map read . tail . words