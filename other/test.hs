import Data.List
import Data.Function

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n -1)

solve7 :: [Int] -> Int 
solve7 (k:xs) = length [ () | (i, xi) <- zip [0 ..] xs,
                              (j, xj) <- zip [0 ..] xs,
                              i < j,
                              (xi + xj) `mod` k== 0 ]

solve72 :: [Int] -> Int
-- was    head . head. sortBy (flip compare `on` length) . group . sort
solve72 = head . minimumBy (flip compare `on` length) . group . sort   

solve8 :: [Int] -> Int 
solve8 = sum . map (\xs -> length xs `div` 2) . group . sort

printPounds :: Int -> String 
printPounds 0 = ""
printPounds x = "#" ++ printPounds (x - 1)

paddWithSpaces :: Int -> String -> String
paddWithSpaces x str
    | length str < x = paddWithSpaces x (' ':str)
    | otherwise = str


solvePound :: Int -> String
solvePound x = unlines $ map (paddWithSpaces x . printPounds) [1..x]

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

getPairs :: [Int] -> [[Int]]
getPairs [] = []
getPairs (x:xs) = [x] : getPairs xs

funt :: [Int] -> [[Int]]
funt [] = []
funt (x:xs) = map (\y -> [x,y]) xs ++ funt xs
