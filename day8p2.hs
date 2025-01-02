import Data.Map.Strict (fromList, Map, (!))

main = do
    contents <- readFile "inputs/day8.txt"
    let moves = cycle $ head $ lines contents
        network = fromList $ map readLine $ drop 2 $ lines contents
        starts = filter (\ x -> last x == 'A') $ map (take 3) $ drop 2 $ lines contents
        cycles = map (calc network moves 0) starts
        lists = map (\ (x:y:_) -> enumFromThen x y) cycles

    print $ (* 271) $ foldr1 lcm $ map ((`div` 271) . head) lists
    print $ take 1 $ foldr1 matches lists

    --print $ calc network moves starts 0

readLine :: String -> (String, (String, String))
readLine s = (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))

calc :: Map String (String, String) -> [Char] -> Int -> String -> [Int]
calc m (x:xs) acc t
    | last t == 'Z' = acc : calc m xs (acc + 1) (f $ m ! t)
    | otherwise = calc m xs (acc + 1) (f $ m ! t)
    where f = if x == 'L' then fst else snd

difference :: [Int] -> [Int]
difference (x:y:xs) = y - x : difference (y : xs)

matches' :: [Int] -> [Int] -> [Int]
matches' (x:xs) (y:ys)
    | x == y = x : matches' xs ys
    | x < y = matches' xs (y : ys)
    | x > y = matches' (x : xs) ys

matches :: [Int] -> [Int] -> [Int]
matches x y = enumFromThen a b
    where (a:b:_) = matches' x y


