import Util
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import GHC.Data.ShortText (ShortText(contents))
import Numeric (readHex)

data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
    contents <- readFile "inputs/day18.txt"
    let input = map readLine' $ lines contents
        i = input ++ [head input]
        c = corners input (0, 0)
        l = sum $ map snd input
        c' = tail $ init c
        s = sum $ map triangle c'
        a = (s + l) `div` 2

    print (a + 1)

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


countArea :: [(Direction, Int)] -> Int
countArea [] = 0
countArea ((d, n):[]) = 0
countArea ((d1, n1):(d2, n2):xs) = m * n1 * n2 + countArea ((d2, n2):xs)
    where m = case (d1, d2) of
            (North, East) -> 1
            (East, South) -> 1
            (South, West) -> 1
            (West, North) -> 1
            (East, North) -> -1
            (South, East) -> -1
            (West, South) -> -1
            (North, West) -> -1

corners :: [(Direction, Int)] -> (Int, Int) -> [(Direction, Int, Int, Int)]
corners [] _ = []
corners ((d, m):xs) (y, x) = (d, m, y, x) : corners xs (y', x')
    where (y', x') = case d of
            North -> (y-m, x)
            East -> (y, x+m)
            South -> (y+m, x)
            West -> (y, x-m)


triangle :: (Direction, Int, Int, Int) -> Int
triangle (d, m, y, x) = if x' * y' > 0 then (abs x') * (abs y') else -1 * (abs x') * (abs y')
    where x' = case d of
            North -> x
            South -> x
            East -> -m
            West -> m
          y' = case d of
            North -> -m
            South -> m
            East -> y
            West -> y