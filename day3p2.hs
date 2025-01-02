import Data.ByteString (find)
import Data.Binary.Get (label)
main = do
    contents <- readFile "inputs/day3.txt"
    let nums = findNums 0 $ lines contents
        gears = findGears 0 $ lines contents
    print $ sum $ map (`value` nums) gears

findNums :: Int -> [String] -> [(Int, Int, Int, Int)]
findNums _ [] = []
findNums y (x:xs) = f 0 x ++ findNums (y + 1) xs
    where
        f :: Int -> String -> [(Int, Int, Int, Int)]
        f _ [] = []
        f n (x:xs)
            | x `notElem` ['0'..'9'] = f (n+1) xs
            | otherwise = let s = x : takeWhile (`elem` ['0'..'9']) xs
                              v = read s
                              l = length s
                          in (n, n + l - 1, y, v) : f (n + l) (drop (l - 1) xs)

findGears :: Int -> [String] -> [(Int, Int)]
findGears _ [] = []
findGears y (x:xs) = f 0 x ++ findGears (y + 1) xs
    where
        f :: Int -> String -> [(Int, Int)]
        f _ [] = []
        f n ('*':xs) = (n, y) : f (n + 1) xs
        f n (x:xs) = f (n + 1) xs

value :: (Int, Int) -> [(Int, Int, Int, Int)] -> Int
value _ [] = 0
value g l
    | length (filter (adjacent g) l) /= 2 = 0
    | otherwise = product $ map getValue $ filter (adjacent g) l

adjacent :: (Int, Int) -> (Int, Int, Int, Int) -> Bool
adjacent (gx, gy) (x1, x2, y, _)
    | abs (gy - y) > 1 = False
    | (abs (gx - x1) > 1) && (abs (gx - x2) > 1) = False
    | otherwise = True

getValue (_, _, _, v) = v