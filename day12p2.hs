import Util

main = do
    contents <- readFile "inputs/day12.txt"
    let a = map calc $ lines contents
    print a
    print $ sum a

calc :: String -> Int
calc s = row (quintuple' m) (quintuple l)
    where m = takeWhile (/= ' ') s
          l = map read $ splitWhen (==',') $ tail $ dropWhile (/= ' ') s

row :: String -> [Int] -> Int
row [] [] = 1
row [] _ = 0
row ('#':xs) [] = 0 
row ('?':xs) [] = row xs []
row ('.':xs) l = row xs l
row ('#':xs) (y:l)
    | (length xs >= (y-1)) && ('.' `notElem` take (y-1) xs) && (length xs == (y-1) || xs !! (y-1) /= '#') = row (drop y xs) l
    | otherwise = 0
row ('?':xs) l = row ('.':xs) l + row ('#':xs) l

quintuple x = x ++ x ++ x ++ x ++ x

quintuple' :: [Char] -> [Char]
quintuple' x = x ++ ('?':x) ++ ('?':x) ++ ('?':x) ++ ('?':x)
