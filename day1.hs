main = do
    contents <- readFile "inputs/day1.txt"
    print $ sum $ map calc $ lines contents


calc :: String -> Int
calc x = 10 * calcFirst x + calcFirst (reverse x)

calcFirst :: String -> Int
calcFirst x
    | head x `elem` ['a'..'z'] = calcFirst $ tail x
    | otherwise = read [head x] :: Int
    