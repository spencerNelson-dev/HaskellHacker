testInput :: [Int]
testInput = [11, 2, 4, 4, 5, 6, 10, 8, -12]

testString :: String
testString = "4\n1 2 3 4\n4 5 6 4\n7 8 9 4\n"

chop :: Int -> [Int] -> [[Int]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

solve :: [[Int]] -> Int
solve s =
  abs $
    foldl1 (-) $
      concat
        [ [sum $ map (uncurry (!!)) $ zip s [0 ..]],
          [sum $ map (uncurry (!!)) $ zip (map reverse s) [0 ..]]
        ]

main :: IO ()
main = interact $ show . solve . map (map read . words) . tail . lines