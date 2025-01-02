import Data.List (findIndex)
data Direction = North | East | South | West | End deriving (Show, Eq, Ord, Enum)

main = do
    contents <- readFile "inputs/day10.txt"
    let tiles = lines contents
        y = findIndex (elem 'S') tiles
        start'= case y of
                    Just y2 -> (y2, findIndex (== 'S') (tiles !! y2))
                    Nothing -> error "No starting position found"

        start = case start' of
                    (y2, Just x2) -> (y2, x2)
                    _ -> error "No starting position found"

        l = calc tiles start North
    print $ (length l `div` 2)


calc :: [String] -> (Int, Int) -> Direction -> [Direction]
calc tiles (y, x) dir
    | dir == End = []
    | otherwise = dir : calc tiles (y', x') dir'
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
