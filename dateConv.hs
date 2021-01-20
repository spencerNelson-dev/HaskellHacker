import Data.List
import Text.Printf

sample :: String
sample = "07:05:45PM"

split :: String -> [String]
split t = [init $ init t, reverse $ take 2 $ reverse t]

toInts :: String -> [Int]
toInts = map read . filter ( /= ":") . groupBy (\x y -> x /= ':' && y /= ':')

toTime :: [Int] -> String
toTime = intercalate ":" . map (printf "%02i")

addToTime :: Int -> [Int] -> [Int]
addToTime a [th,tm,ts] = [ a + th, tm, ts]

conversion :: [String] -> String 
conversion [time, p]
    | p == "AM" && hour == 12 = toTime $ addToTime (-12) $ toInts time
    | p == "AM" = time
    | p == "PM" && hour == 12 = time
    | p == "PM" = toTime $ addToTime 12 $ toInts time
    where hour = head (toInts time)


solve :: String -> String 
solve = conversion . split

main :: IO ()
main = interact solve