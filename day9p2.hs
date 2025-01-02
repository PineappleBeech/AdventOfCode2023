
main = do
    contents <- readFile "inputs/day9.txt"
    let lists = map (map read . words) $ lines contents
    
    print $ sum $ map calc lists

calc :: [Int] -> Int
calc x
    | all (== 0) x = 0
    | otherwise = head x - calc (difference x)

difference :: [Int] -> [Int]
difference [] = []
difference [x] = []
difference (x:y:xs) = y - x : difference (y : xs)