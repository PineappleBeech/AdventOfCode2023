import Data.List (findIndex)
import Text.XHtml (area)
import Graphics.Win32 (hS_HORIZONTAL)
data Direction = North | East | South | West | End deriving (Show, Eq, Ord, Enum)

main = do
    contents <- readFile "inputs/day10.txt"
    let tiles = lines contents
        y = findIndex (elem 'S') tiles
        start' = case y of
                    Just y2 -> (y2, findIndex (== 'S') (tiles !! y2))
                    Nothing -> error "No starting position found"

        start = case start' of
                    (y2, Just x2) -> (y2, x2)
                    _ -> error "No starting position found"

        l = calc tiles start North
        path = (\ (d, x, y) -> (x, y)) <$> l
        h = length (lines contents)
        w = length (head $ lines contents)
        totalArea = (h + 2) * (w + 2)
        --area = totalArea - length path - (fillArea path (map (\ x -> [0..w+2]) [0..h+2]))
        --area = fillArea' path $ pad (build (\ x -> if x `elem` path then -1 else 0) (h, w))


        inPath = build (`elem` path) (h, w)
        area = sum $ map (\ (x, y) -> processRow x y False False) $ zip inPath tiles
    print area


calc :: [String] -> (Int, Int) -> Direction -> [(Direction, Int, Int)]
calc tiles (y, x) dir
    | dir == End = []
    | otherwise = (dir, y, x) : calc tiles (y', x') dir'
    where
        (y', x') = case dir of
                    North -> (y - 1, x)
                    East -> (y, x + 1)
                    South -> (y + 1, x)
                    West -> (y, x - 1)

        dir' = case (dir, tiles !! y' !! x') of
                    (North, '|') -> North
                    (North, '7') -> West
                    (North, 'F') -> East
                    (South, '|') -> South
                    (South, 'L') -> East
                    (South, 'J') -> West
                    (East, '-') -> East
                    (East, 'J') -> North
                    (East, '7') -> South
                    (West, '-') -> West
                    (West, 'L') -> North
                    (West, 'F') -> South
                    (_, 'S') -> End

build :: ((Int, Int) -> a) -> (Int, Int) -> [[a]]
build f (y2, x2) = map (\y -> map (\x -> f (y, x)) [0..x2-1]) [0..y2-1]

fillArea :: [(Int, Int)] -> [[Int]] -> [Int]
fillArea path area
    | count == count' = [count]
    | otherwise = count : fillArea path area'
    where h = length area
          w = length (head area)
          area' = build (fuzzy area path) (h, w)
          count = count2 area
          count' = count2 area'

fuzzy :: [[Int]] -> [(Int, Int)] -> (Int, Int) -> Int
fuzzy area path (y, x)
    | y == 0 || x == 0 = 1
    | (y + 1, x + 1) `elem` path = -1
    | any check [(y - 1, x),
                 (y, x - 1), (y, x), (y, x + 1),
                 (y + 1, x)] = 1
    | otherwise = 0
    where check (y, x) = case area !? y of
                            Just row -> case row !? x of
                                            Just x -> x == 1
                                            Nothing -> False
                            Nothing -> False

count2 :: [[Int]] -> Int
count2 area = sum $ map (sum . map f) area
    where f x = if x == 0 then 1 else 0


(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x:xs) 0 = Just x
(!?) (x:xs) n = xs !? (n - 1)

blank :: (Int, Int) -> [[Int]]
blank (h, w) = map (\x -> map (\y -> 0) [0..w]) [0..h]

fillArea' :: [(Int, Int)] -> [[Int]] -> [Int]
fillArea' path area
    | count == count' = [count]
    | otherwise = count : fillArea' path area'
    where h = length area
          w = length (head area)
          padded = pad area
          area' = map calcRow $ group padded
          count = count2 area
          count' = count2 area'

          

group :: [a] -> [(a,a,a)]
group (x:y:z:xs) = (x,y,z) : group (y:z:xs)
group _ = []

calcRow :: ([Int], [Int], [Int]) -> [Int]
calcRow ((x:xs), (y:(-1):ys), (z:zs)) = -1 : calcRow (xs, (-1):ys, zs)
calcRow ((x1:x2:x3:xs), (y1:y2:y3:ys), (z1:z2:z3:zs)) = if any (== 1) [x2, y1, y2, y3, z2]
                                                            then 1 : calcRow (x2:x3:xs, y2:y3:ys, z2:z3:zs)
                                                            else y2 : calcRow (x2:x3:xs, y2:y3:ys, z2:z3:zs)
calcRow _ = []


pad :: [[Int]] -> [[Int]]
pad area = map (\x -> 1 : x ++ [1]) (a : area ++ [a])
    where a = replicate (length (head area)) 1

padFalse :: [[Bool]] -> [[Bool]]
padFalse area = map (\x -> False : x ++ [False]) (a : area ++ [a])
    where a = replicate (length (head area)) False

pprint :: [[Int]] -> IO ()
pprint area = mapM_ print area'
    where area' = map (map f) area
          f x = if x == 0 then ' ' else if x == 1 then '#' else 'X'

processRow :: [Bool] -> [Char] -> Bool -> Bool -> Int
processRow (False:xs) (y:ys) inside False
    | inside = 1 + processRow xs ys inside False
    | otherwise = processRow xs ys inside False
processRow (True:xs) (y:ys) inside False
    | y `elem` "|S" = processRow xs ys (not inside) False
    | y `elem` "L" = processRow xs ys inside True
    | y `elem` "F" = processRow xs ys (not inside) True
    | otherwise = error "Invalid input"
processRow (True:xs) (y:ys) inside True
    | y `elem` "-" = processRow xs ys inside True
    | y `elem` "J" = processRow xs ys inside False
    | y `elem` "7" = processRow xs ys (not inside) False
    | otherwise = error "Invalid input"
processRow [] [] False False = 0
