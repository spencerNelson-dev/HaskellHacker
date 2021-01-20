import Data.List (nub)

testInput :: [Int]
testInput = [0, 1, 0, 0, 0, 1, 0]

jumpPath :: [Int] -> [Int]
jumpPath = undefined 

canJump2 :: [Int] -> Int -> [Int] -> [Int]
canJump2 xs pos acc
    | pos < length xs && (xs !! pos) == 0 = canJump2 xs (pos + 2) (pos:acc)
    | (pos - 1) < length xs && (xs !! (pos - 1)) == 0 = canJump2 xs (pos - 1) ((pos - 1):acc)
    | otherwise = nub $ reverse acc

solve :: [Int] -> Int 
solve xs = length (canJump2 xs 0 []) - 1

main :: IO ()
main = interact $ show . solve . map read . tail . words