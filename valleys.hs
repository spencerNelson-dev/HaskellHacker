import Data.List

relativeChange :: Char -> Int 
relativeChange 'U' = 1
relativeChange 'D' = -1

solve :: String -> Int 
solve =
    length . filter (all (<0)) . groupBy (\x y -> x /= 0 && y/= 0 ) . scanl (+) 0 . map relativeChange

main :: IO ()
main = interact $ show . solve . head . tail . words