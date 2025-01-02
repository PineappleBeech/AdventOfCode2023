import Util (splitWhen)
import Data.List (transpose)

main = do
    contents <- readFile "inputs/day13.txt"
    let maps = splitWhen (== "") $ lines contents

    print $ sum $ map calc maps
    print $ map calc maps

calc :: [String] -> Int
calc m = case a of
            Just x -> 100 * x
            Nothing -> calc (transpose m) `div` 100
    where a = findPair m 1

findPair :: Eq a => [[a]] -> Int -> Maybe Int
findPair l x
    | x == length l = Nothing
    | sum (zipWith diff (reverse $ take x l) (drop x l)) == 1 = Just x
    | otherwise = findPair l (x + 1)

diff :: Eq a => [a] -> [a] -> Int
diff [] [] = 0
diff (x:xs) (y:ys)
    | x == y = diff xs ys
    | otherwise = 1 + diff xs ys

