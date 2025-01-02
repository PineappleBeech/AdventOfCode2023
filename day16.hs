import Data.Maybe (isNothing)
import Data.List (nub, sort)
import Data.Text.Unsafe (iter)
data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
    contents <- readFile "inputs/day16.txt"
    let input = lines contents
        directions :: [[[Direction]]] = ([South] : replicate (length (head input) - 1) []) : replicate (length input - 1) (replicate (length (head input)) [])
        l = iterate (run input) directions

    print $ calc input directions
    print $ length $ calc input directions
    --mapM_ (putStrLn . pprint) (take 34 l)
    

calc :: [String] -> [[[Direction]]] -> [Int]
calc m d
    | d == d' = [count d]
    | otherwise = count d : calc m d'
    where d' = run m d


count ::  [[[Direction]]] -> Int
count d = sum $ map (length . filter (not . null)) d

run :: [String] -> [[[Direction]]] -> [[[Direction]]]
run m d = build f (length m, length (head m))
    where f (y, x) = sort $ nub ((d !! y !! x) ++ n ++ e ++ s ++ w)
            where p = points d
                  m' = m !! y !! x
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

points :: [[[Direction]]] -> (Int, Int) -> Direction -> Bool
points m (y, x) d = d `maybeElem` (m !? y ?? x)


(!?) :: [a] -> Int -> Maybe a
(!?) xs i
    | i < 0 = Nothing
    | i >= length xs = Nothing
    | otherwise = Just (xs !! i)

(??) :: Maybe [a] -> Int -> Maybe a
(??) Nothing _ = Nothing
(??) (Just xs) i = xs !? i

maybeElem :: Eq a => a -> Maybe [a] -> Bool
maybeElem _ Nothing = False
maybeElem x (Just xs) = x `elem` xs

build :: ((Int, Int) -> a) -> (Int, Int) -> [[a]]
build f (y2, x2) = map (\y -> map (\x -> f (y, x)) [0..x2-1]) [0..y2-1]

pprint :: [[[Direction]]] -> String
pprint d = unlines (map (map f) d) ++ "\n"
    where f [] = '.'
          f [North] = '^'
          f [East] = '>'
          f [South] = 'v'
          f [West] = '<'
          f l = head (show (length l))
