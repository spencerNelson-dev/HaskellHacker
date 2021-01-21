import Data.List

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

fast :: [Int] -> Int
fast = length . filter(\[x,y] -> (x + y) `mod` 3 == 0 ) . filter(\x -> length x == 2) . subsequences

solve :: [Int] -> Int 
solve (m:xs) =  length . filter(\[x,y] -> (x + y) `mod` m == 0 ) . filter(\x -> length x == 2) $ subsequences xs

-- solve2 :: [Int] -> Int 
-- solve2 xs =   group $ sort $ map (`mod` 3) xs
--     where m = head xs

fact :: Int -> Int 
fact n = product [1..n]

choose :: Int -> Int -> Int 
choose n r = (fact n) `div` ((fact r) * (n -r))

main :: IO ()
main = interact $ show . solve . map read . tail . words