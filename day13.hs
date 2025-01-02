import Util (splitWhen)
import Data.List (transpose)
import System.Win32 (xBUTTON1)

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

findPair :: Eq a => [a] -> Int -> Maybe Int
findPair l x
    | x == length l = Nothing
    | all (\ (x, y) -> x == y) $ zip (reverse $ take x l) (drop x l) = Just x
    | otherwise = findPair l (x + 1)

