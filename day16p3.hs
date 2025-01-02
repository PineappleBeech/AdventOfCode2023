import Data.Maybe (isNothing, isJust)
import Data.List (nub, sort)
import Data.Array (Array, (!), bounds, array, elems)
import Data.Set (Set, empty, insert, union, fromList, toList, singleton, showTree)
data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
    contents <- readFile "inputs/day16.txt"
    let input' = lines contents
        input ::  Array (Int, Int) Char = array ((0, 0), (length input' - 1, length (head input') - 1)) [((y, x), input' !! y !! x) | y <- [0..length input' - 1], x <- [0..length (head input') - 1]]
        s = search input (0, 0) East empty
        s' = fromList (map (\(y, x, d) -> (y, x)) (toList s))
        startFrom (y, x) d = length $ fromList (map (\(y, x, d) -> (y, x)) (toList (search input (y, x) d empty)))

        ((y1, x1), (y2, x2)) = bounds input

        east = maximum [startFrom (y, 0) East | y <- [y1..y2]]
        west = maximum [startFrom (y, x2) West | y <- [y1..y2]]
        south = maximum [startFrom (0, x) South | x <- [x1..x2]]
        north = maximum [startFrom (y2, x) North | x <- [x1..x2]]

    --print $ calc input directions
    --print $ length $ calc input directions
    --mapM_ (putStrLn . pprint) (take 34 l)
    --putStrLn $ showTree s
    print east
    print west
    print south
    print north
    print $ maximum [east, west, south, north]
    print $ length s
    print $ length s'


calc :: Array (Int, Int) Char -> Array (Int, Int) [Direction] -> [Int]
calc m d
    | d == d' = [count d]
    | otherwise = count d : calc m d'
    where d' = run m d


count :: Array (Int, Int) [Direction] -> Int
count d = length $ filter (not . null) (elems d)

run :: Array (Int, Int) Char -> Array (Int, Int) [Direction] -> Array (Int, Int) [Direction]
run m d = buildBounded f (bounds d)
    where f (y, x) = sort $ nub ((d ! (y, x)) ++ n ++ e ++ s ++ w)
            where p = points d
                  m' = m ! (y, x)
                  n = if (y-1, x) `p` South then process m' South else []
                  e = if (y, x+1) `p` West then process m' West else []
                  s = if (y+1, x) `p` North then process m' North else []
                  w = if (y, x-1) `p` East then process m' East else []

process :: Char -> Direction -> [Direction]
process '.' d = [d]
process '|' North = [North]
process '|' South = [South]
process '-' East = [East]
process '-' West = [West]
process '|' East = [North, South]
process '|' West = [North, South]
process '-' North = [East, West]
process '-' South = [East, West]
process '/' North = [East]
process '/' South = [West]
process '/' East = [North]
process '/' West = [South]
process '\\' North = [West]
process '\\' South = [East]
process '\\' East = [South]
process '\\' West = [North]

points :: Array (Int, Int) [Direction] -> (Int, Int) -> Direction -> Bool
points m (y, x) d = case ds of
    Nothing -> False
    Just ds' -> d `elem` ds'
    where ds = maybeGet m (y, x)

maybeGet :: Array (Int, Int) a -> (Int, Int) -> Maybe a
maybeGet a (y, x) = if y < 0 || y > y2 || x < 0 || x > x2 then Nothing else Just (a ! (y, x))
    where ((y1, x1), (y2, x2)) = bounds a

build :: ((Int, Int) -> a) -> (Int, Int) -> Array (Int, Int) a
build f (y2, x2) = array ((0, 0), (y2-1, x2-1)) [((y, x), f (y, x)) | y <- [0..y2-1], x <- [0..x2-1]]

buildBounded :: ((Int, Int) -> a) -> ((Int, Int), (Int, Int)) -> Array (Int, Int) a
buildBounded f ((y1, x1), (y2, x2)) = array ((y1, x1), (y2, x2)) [((y, x), f (y, x)) | y <- [y1..y2], x <- [x1..x2]]

pprint :: [[[Direction]]] -> String
pprint d = unlines (map (map f) d) ++ "\n"
    where f [] = '.'
          f [North] = '^'
          f [East] = '>'
          f [South] = 'v'
          f [West] = '<'
          f l = head (show (length l))

search :: Array (Int, Int) Char -> (Int, Int) -> Direction -> Set (Int, Int, Direction) -> Set (Int, Int, Direction)
search m (y, x) d s
    | (y, x, d) `elem` s = s
    | otherwise = case maybeGet m (y, x) of
    Nothing -> s
    Just c -> f c d
    where f '|' North = searchDir m (y, x) North s'
          f '|' South = searchDir m (y, x) South s'
          f '-' East = searchDir m (y, x) East s'
          f '-' West = searchDir m (y, x) West s'
          f '|' East = let s'' = searchDir m (y, x) North s' in searchDir m (y, x) South s''
          f '|' West = let s'' = searchDir m (y, x) North s' in searchDir m (y, x) South s''
          f '-' North = let s'' = searchDir m (y, x) East s' in searchDir m (y, x) West s''
          f '-' South = let s'' = searchDir m (y, x) East s' in searchDir m (y, x) West s''
          f '/' North = searchDir m (y, x) East s'
          f '/' South = searchDir m (y, x) West s'
          f '/' East = searchDir m (y, x) North s'
          f '/' West = searchDir m (y, x) South s'
          f '\\' North = searchDir m (y, x) West s'
          f '\\' South = searchDir m (y, x) East s'
          f '\\' East = searchDir m (y, x) South s'
          f '\\' West = searchDir m (y, x) North s'
          f '.' d = searchDir m (y, x) d s'
          s' = insert (y, x, d) s

searchDir :: Array (Int, Int) Char -> (Int, Int) -> Direction -> Set (Int, Int, Direction) -> Set (Int, Int, Direction)
searchDir m (y, x) North = search m (y-1, x) North
searchDir m (y, x) South = search m (y+1, x) South
searchDir m (y, x) East = search m (y, x+1) East
searchDir m (y, x) West = search m (y, x-1) West
