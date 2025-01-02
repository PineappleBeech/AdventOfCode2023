import Util
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Numeric (readHex)

data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
    contents <- readFile "inputs/test18.txt"
    let input = map readLine $ lines contents

    print $ map fst $ take 10 $ iterate cutJoin (0, input)

readLine :: String -> (Direction, Int)
readLine (d:' ':xs) = (d', read $ takeWhile (/= ' ') xs)
    where d' = case d of
            'U' -> North
            'R' -> East
            'D' -> South
            'L' -> West

readLine' :: String -> (Direction, Int)
readLine' s = (d', n)
    where s' = tail $ dropWhile (/= '#') s
          d = head $ drop 5 s'
          n = fst $ head $ readHex $ take 5 s'
          d' = case d of
            '3' -> North
            '0' -> East
            '1' -> South
            '2' -> West


holes ::  [(Direction, Int)] -> (Int, Int) -> [(Int, Int)]
holes [] _ = []
holes ((d, 0):xs) (y, x) = holes xs (y, x)
holes ((d, n):xs) (y, x) = (y', x') : holes ((d, n-1):xs) (y', x')
    where (y', x') = case d of
            North -> (y-1, x)
            East -> (y, x+1)
            South -> (y+1, x)
            West -> (y, x-1)

toGrid :: [(Int, Int)] -> [[Int]]
toGrid holes = grid
    where (y1, x1) = (minimum $ map fst holes, minimum $ map snd holes)
          (y2, x2) = (maximum $ map fst holes, maximum $ map snd holes)
          grid = build (\(y, x) -> if (y + y1, x + x1) `elem` holes then -1 else 0) (y2-y1+1, x2-x1+1)

calc :: [[Int]] ->  [Int]
calc d
    | d == d' = [count d]
    | otherwise = count d : calc d'
    where d' = run d


run :: [[Int]] -> [[Int]]
run = iterArea f 1
    where f v (n, e, s, w)
            | v == -1 = -1
            | any (== 1) [v, n, e, s, w] = 1
            | otherwise = 0


count :: [[Int]] -> Int
count d = sum $ map (length . filter (/= 1)) d

pprint :: [[Int]] -> IO ()
pprint = mapM_ (putStrLn . concatMap f)
    where f 0 = "."
          f 1 = "O"
          f (-1) = "#"

countWall :: [[Int]] -> Int
countWall = sum . map (length . filter (== -1))

countTurns :: [(Direction, Int)] -> Int
countTurns [] = 0
countTurns [_] = 0
countTurns ((x, _):t@(y, _):xs) = case (x, y) of
    (North, East) -> 1 + countTurns (t:xs)
    (East, South) -> 1 + countTurns (t:xs)
    (South, West) -> 1 + countTurns (t:xs)
    (West, North) -> 1 + countTurns (t:xs)
    (East, North) -> -1 + countTurns (t:xs)
    (South, East) -> -1 + countTurns (t:xs)
    (West, South) -> -1 + countTurns (t:xs)
    (North, West) -> -1 + countTurns (t:xs)

cut :: [(Direction, Int)] -> (Int, [(Direction, Int)])
cut [] = (0, [])
cut [x] = (0, [x])
cut ((d1, n1):(d2, n2):xs)
    | b = ((n1 - 1) * (n2 - 1) + fst c, (d2, n2):snd c)
    | otherwise = (fst c, (d1, n1):snd c)
    where b = case (d1, d2) of
            (North, East) -> True
            (East, South) -> True
            (South, West) -> True
            (West, North) -> True
            _ -> False
          c = if b then cut ((d1, n1):xs) else cut ((d2, n2):xs)

expand :: [(Direction, Int)] -> (Int, [(Direction, Int)])
expand [] = (0, [])
expand [x] = (0, [x])
expand ((d1, n1):(d2, n2):xs)
    | b = ((-1) * (n1) * (n2) + fst c, (d2, n2):snd c)
    | otherwise = (fst c, (d1, n1):snd c)
    where b = case (d1, d2) of
            (East, North) -> True
            (South, East) -> True
            (West, South) -> True
            (North, West) -> True
            _ -> False
          c = if b then expand ((d1, n1):xs) else expand ((d2, n2):xs)


join :: [(Direction, Int)] -> (Int, [(Direction, Int)])
join [] = (0, [])
join [x] = (0, [x])
join ((d1, n1):(d2, n2):xs)
    | d1 == d2 = join ((d1, n1 + n2):xs)
    | d1 == opposite d2 && n1 == n2 = let c = join xs in (fst c + n1, snd c)
    | d1 == opposite d2 = let c = join ((if n1 > n2 then d1 else d2, abs (n1-n2)):xs) in (fst c + min n1 n2, snd c)
    | otherwise = let c = join ((d2, n2):xs) in (fst c, (d1, n1):snd c)

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

cutJoin :: (Int, [(Direction, Int)]) -> (Int, [(Direction, Int)])
cutJoin (n, xs) = (n + fst j + fst c, snd j)
    where c = cut xs
          j = join $ snd c
