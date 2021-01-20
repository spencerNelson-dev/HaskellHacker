import Data.List

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

sums :: [Int] -> [Int]
sums = sort . map sum . filter (\x -> length x ==4) . subsets 

solve :: [Int] -> [Int]
solve s = [minimum $ sums s, maximum $ sums s]

main :: IO ()
main = interact $ unwords . map show . solve . map read. words