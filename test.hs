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

