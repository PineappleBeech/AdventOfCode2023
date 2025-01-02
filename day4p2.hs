import Util

main = do
    contents <- readFile "inputs/day4.txt"
    print $ calc (lines contents) $ replicate (length $ lines contents) 1

calc :: [String] -> [Int] -> Int
calc [] _ = 0
calc s l = head l + calc (tail s) (map (+ head l) (take count (tail l)) ++ drop count (tail l))
    where t = drop 9 $ head s
          (a, b) = splitFirst (== '|') t
          c = ints a
          d = ints b
          count = length (filter (`elem` c) d)

ints :: String -> [Int]
ints "" = []
ints " " = []
ints s = read (take 3 s) : ints (drop 3 s)