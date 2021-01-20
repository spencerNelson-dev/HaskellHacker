import Data.List

solve :: [Int] -> Int 
solve = length . last . group . sort

main :: IO ()
main = interact $ show . solve . map read . tail . words