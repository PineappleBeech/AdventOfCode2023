
import Util (iterArea)
import Data.ByteString (find)
import Data.List ( findIndex, findIndices, elemIndices )

main = do
    contents <- readFile "inputs/day21.txt"
    let m = lines contents
        w = length $ head m
        h = length m
        y = head $ findIndices ('S' `elem`) m
        x = head $ elemIndices 'S' $ m !! y
        blank = replace (y, x) '#' m
        d = 26501365
        m' = expandFrom blank 65 (65, 65)
        a = count $ expandFrom blank 65 (65, 65)
        b = sum $ map (count . expandFrom blank 64) [(0, 0), (h-1, 0), (0, w-1), (h-1, w-1)]
        n = d `div` 131
        b' = ((2 * n) + 1) ^ 2 `div` 2
        a' = b' + 1

    mapM_ putStrLn m'
    mapM_ putStrLn $ expandFrom blank 64 (0, 0)
    print (h, w)
    print $ count m'
    print ((a * a') +  (b * b'))


expand :: Int -> [String] -> [String]
expand 0 m = m
expand n m = expand (n - 1) $ iterArea f '.' m
    where f v (n, e, s, w)
            | v == '#' = '#'
            | any (`elem` "SO") [n, e, s, w] = 'O'
            | otherwise = '.'

count :: [String] -> Int
count = length . filter (== 'O') . concat

replace :: (Int, Int) -> a -> [[a]] -> [[a]]
replace (y, x) v m = take y m ++ [take x (m !! y) ++ [v] ++ drop (x + 1) (m !! y)] ++ drop (y + 1) m

expandFrom :: [String] -> Int -> (Int, Int) -> [String]
expandFrom m n (y, x) = expand n $ replace (y, x) 'O' m