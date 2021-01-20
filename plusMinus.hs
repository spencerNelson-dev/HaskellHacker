import Text.Printf

testInput :: [Float]
testInput = [-4, 3, -9, 0, 4, 1]

solve :: [Int] -> [Float]
solve xs = [p, n, z]
    where
        p = (fromIntegral $ length $ filter (> 0) xs :: Float) / xsLength
        n = (fromIntegral $ length $ filter (< 0) xs :: Float) / xsLength
        z = (fromIntegral $ length $ filter (== 0) xs :: Float) / xsLength
        xsLength = fromIntegral (length xs) :: Float

main :: IO ()
main =  interact $ unlines . map (printf "%6f") . solve . map read . tail . words