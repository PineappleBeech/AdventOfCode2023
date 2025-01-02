import Util
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)

data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
    contents <- readFile "inputs/day18.txt"
    let input = map readLine $ lines contents
        grid = toGrid $ holes input (0, 0)

    print $ countWall grid
    print $ calc grid

readLine :: String -> (Direction, Int)
readLine (d:' ':xs) = (d', read $ takeWhile (/= ' ') xs)
    where d' = case d of
            'U' -> North
            'R' -> East
            'D' -> South
            'L' -> West

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
