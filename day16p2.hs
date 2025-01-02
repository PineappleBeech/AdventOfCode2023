import Data.Maybe (isNothing, isJust)
import Data.List (nub, sort)
import Data.Array (Array, (!), bounds, array, elems)
data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
    contents <- readFile "inputs/day16.txt"
    let input' = lines contents
        input ::  Array (Int, Int) Char = build (\(y, x) -> input' !! y !! x) (length input', length (head input'))
        directions' :: [[[Direction]]] = (process (head $ head input') East : replicate (length (head input') - 1) []) : replicate (length input' - 1) (replicate (length (head input')) [])
        directions :: Array (Int, Int) [Direction] = build (\(y, x) -> directions' !! y !! x) (length input', length (head input'))
        l = iterate (run input) directions

    print $ calc input directions
    print $ length $ calc input directions
    --mapM_ (putStrLn . pprint) (take 34 l)


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
maybeGet a (y, x) = if y < 0 || y >= y2 || x < 0 || x >= x2 then Nothing else Just (a ! (y, x))
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
