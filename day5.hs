import Util

main = do
    contents <- readFile "inputs/day5.txt"
    let l = lines contents
        seeds :: [Int] = map read $ splitWhen (== ' ') $ drop 7 $ head l
        f = calc $ drop 2 l
    
    print $ minimum $ map f seeds

calc :: [String] -> Int -> Int
calc [] x = x
calc l x = calc ms (mapValue (tail m) x)
    where (m,ms) = splitFirst (== "") l

mapValue :: [String] -> Int -> Int
mapValue [] x = x
mapValue (s:ss) x
    | x >= source && x < source + l = dest + x - source
    | otherwise = mapValue ss x
    where [dest, source, l] = map read $ splitWhen (== ' ') s
