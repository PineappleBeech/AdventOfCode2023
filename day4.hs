import Util
import Language.Haskell.TH (safe)

main = do
    contents <- readFile "inputs/day4.txt"
    print $ sum $ map calc $ lines contents

calc :: String -> Int
calc s = if count == 0 then 0 else 2 ^ (count - 1)
    where t = drop 9 s
          (a, b) = splitFirst (== '|') t
          c = ints a
          d = ints b
          count = length (filter (`elem` c) d)

ints :: String -> [Int]
ints "" = []
ints " " = []
ints s = read (take 3 s) : ints (drop 3 s)