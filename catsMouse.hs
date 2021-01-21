chop :: Int -> [Int] -> [[Int]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

dis :: Int -> Int -> Int 
dis x y = abs ( x - y)

winner :: [Int] -> String 
winner [a, b, c] 
    | dis a c == dis b c = "Mouse C"
    | dis a c > dis b c = "Cat B"
    | otherwise = "Cat A"

solve :: [[Int]] -> [String]
solve = map winner

main :: IO ()
main = interact $ unlines . solve . chop 3 . map read . tail . words