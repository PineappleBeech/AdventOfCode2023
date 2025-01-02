import Util

main = do
    contents <- readFile "inputs/day2.txt"
    print $ sum $ map calc $ lines contents

calc :: String -> Int
calc "" = 0
calc s = count "red" x * count "blue" x * count "green" x
    where (_, x) = splitFirst (== ':') s

valid :: String -> Bool
valid "" = True
valid s = check x && valid y
    where
        (x, y) = splitFirst (\ c -> c == ';' || c == ',') s

check :: String -> Bool
check (_:s)
    | y == "red" = read x <= 12
    | y == "green" = read x <= 13
    | y == "blue" = read x <= 14
    | otherwise = False
    where
        (x, y) = splitFirst (== ' ') s

count :: String -> String -> Int
count _ "" = 0
count s (_:x)
    | c == s = max (read n) $ count s ys
    | otherwise = count s ys
    where (y, ys) = splitFirst (\ c -> c == ';' || c == ',') x
          (n, c) = splitFirst (== ' ') y