import Data.List (transpose, span)
main = do
    contents <- readFile "inputs/day14.txt"
    let rocks = transpose $ lines contents
    print $ sum $ map calc rocks

calc :: String -> Int
calc [] = 0
calc ('#':xs) = calc xs
calc x = count l (length $ filter (== 'O') a) + calc b
    where (a, b) = span (/= '#') x
          l = length x

count :: Int ->  Int -> Int
count _ 0 = 0
count x y = x + count (x-1) (y-1)
