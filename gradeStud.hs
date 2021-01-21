
roundTo5 :: Int -> Int 
roundTo5 x
    | x >= 38 && (m5 - x) < 3 = m5
    | otherwise  = x
    where m5 = x + (5 - x `mod` 5)

solution :: [Int] -> [Int]
solution = map roundTo5

main :: IO ()
main = interact $ unlines . map show . solution . map read . tail . words