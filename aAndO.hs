-- Apple and Orange

testInput :: [Int]
testInput = [7, 11,     -- s, t
             5, 15,     -- a, b
             3, 2,      -- a, o
             -2, 2, 1,  -- a
             5, -6]     -- o

solve :: [Int] -> [Int]
solve (s:t:a:b:m:_:rest) = [as, os]
    where as = length $ filter (\x -> s <= x && x <= t) $ map (+a) $ take m rest
          os = length $ filter (\x -> s <= x && x <= t) $ map (+b) $ drop m rest


main :: IO ()
main = interact $ unlines . map show . solve . map read . words