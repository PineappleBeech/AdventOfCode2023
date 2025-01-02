import Util

main = do
    contents <- readFile "inputs/day2.txt"
    print $ sum $ map calc $ lines contents

calc :: String -> Int
calc "" = 0
calc s
    | valid y = read $ drop 5 x
    | otherwise = 0
    where
        (x, y) = splitFirst (== ':') s

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