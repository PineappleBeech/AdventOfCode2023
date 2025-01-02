main = do
    contents <- readFile "inputs/day3.txt"
    print $ sum $ map calc $ group $ pad $ lines contents

group :: [String] -> [(String, String, String)]
group [] = []
group [x] = []
group [x, y] = []
group (x:y:z:xs) = (x, y, z) : group (y:z:xs)

calc :: (String, String, String) -> Int
calc ([], [], []) = 0
calc ('.':'.':xs, '.':'.':ys, '.':'.':zs) = calc ('.':xs, '.':ys, '.':zs)
calc (x1:x2:xs, y1:y2:ys, z1:z2:zs)
    | y2 `notElem` ['1'..'9'] = calc (x2:xs, y2:ys, z2:zs)
    | otherwise = let s = y2 : takeWhile (`elem` ['0'..'9']) ys
                      n = read s
                      l = length s
                  in if any (\ x -> x `notElem` ['0'..'9'] && x /= '.') (x1:x2:y1:z1:z2:(head $ drop (l - 1) ys):(take l xs ++ take l zs))
                        then n + calc (drop (l - 1) xs, drop (l - 1) ys, drop (l - 1) zs)
                        else calc (drop (l - 1) xs, drop (l - 1) ys, drop (l - 1) zs)
calc (".", ".", ".") = 0

pad :: [String] -> [String]
pad l = replicate n '.' : map (\ x -> '.' : x ++ ".") l ++ [replicate n '.']
    where n = 2 + length (head l)