
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
        --d = 131 + 131 + 65
        m' = expandFrom blank 65 (65, 65)
        a = count $ expandFrom blank 65 (65, 65)
        {-
        c1 = sum $ map (count . expandFrom blank 64) [(0, 0), (h-1, 0), (0, w-1), (h-1, w-1)]
        c2 = sum $ map (count . expandFrom blank (64+131)) [(0, 0), (h-1, 0), (0, w-1), (h-1, w-1)]
        e = sum $ map (count . expandFrom blank 130) [(65, 0), (0, 65), (65, w-1), (h-1, 65)]
        fb = count $ expandFrom blank 141 (65, 65)
        fa = count $ expandFrom blank 140 (65, 65)
        -}
        --(fb, fa, c1, c2, e) = (7406,7282,3769,25545,21855)
        (fa, fb) = (7282, 7406)
        c1 = sum $ map (count . expandFrom blank 64) [(0, 0), (h-1, 0), (0, w-1), (h-1, w-1)]
        c2 = sum $ map (count . expandFrom blank (64+131)) [(0, 0), (h-1, 0), (0, w-1), (h-1, w-1)]
        q = expandFrom (triple blank) (65+131) (65+131, 65+131)
        e = sum $ map count $ map (\x -> section x q) [(1, 0), (0, 1), (1, 2), (2, 1)]

        n = d `div` 131
        num = ((n ^ 2) * fa) + (((n - 1) ^ 2) * fb) + (n * c1) + ((n - 1) * c2) + e
        num' = ((n ^ 2) * fb) + (((n - 1) ^ 2) * fa) + (n * c1) + ((n - 1) * c2) + e
        _ = (7406,7282,3769,25545,21855)
        c = count $ expandFrom blank 64 (0, 0)
        c' = count $ expandFrom blank (64+131) (0, 0)
        south = count $ expandFrom blank 130 (0, 65)
        east = count $ expandFrom blank 130 (65, 0)

    --mapM_ putStrLn m'
    --mapM_ putStrLn $ expandFrom blank 130 (65, 0)
    --mapM_ putStrLn $ expandFrom blank 64 (0, 0)
    --mapM_ putStrLn $ combine m1 m2
    print (h, w)
    --print $ count m'
    --print ((a * a') +  (b * b'))
    --print (fa, fb, c1, c2, e)
    --print (fb + fa + fa + c + c + c' + south + east)
    --mapM_ putStrLn q
    print (count $ section (1, 1) q, fb)
    print num
    print num'


    print $ count q

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

combine :: [String] -> [String] -> [String]
combine = zipWith (zipWith (\ c1 c2 -> if c1 == 'O' || c2 == 'O' then 'O' else c1))

triple :: [[a]] -> [[a]]
triple l = l' ++ l' ++ l'
    where l' = map (\x -> x ++ x ++ x) l

quintuple :: [[a]] -> [[a]]
quintuple l = l' ++ l' ++ l' ++ l' ++ l'
    where l' = map (\x -> x ++ x ++ x ++ x ++ x) l

section' :: (Int, Int) -> (Int, Int) -> [[a]] -> [[a]]
section' (y1, x1) (y2, x2) m = map (take (x2 - x1) . drop x1) $ take (y2 - y1) $ drop y1 m

section :: (Int, Int) -> [[a]] -> [[a]]
section (y, x) m = section' (y * 131, x * 131) ((y + 1) * 131, (x + 1) * 131) m

count' :: [String] -> Int
count' = length . filter (== '.') . concat