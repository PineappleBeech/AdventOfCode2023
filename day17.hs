import Util
import Data.List (minimumBy)

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum)


main = do
    contents <- readFile "inputs/day17.txt"
    let m = map (map (\ x ->  (read [x], []))) $ lines contents :: [[(Int, [a])]]
        m2 = ((0, [(South, 0, 0), (East, 0, 0)]) : (tail $ head m)) : tail m

    print $ map (getValue . minimumBy' (\ (_,_,x) (_,_,y) -> compare x y)) $ run m2


run :: [[(Int, [(Direction, Int, Int)])]] -> [[(Direction, Int, Int)]]
run m = (snd $ last $ last m) : run m'
    where m' = iterArea calc (0, []) m


calc :: (Int, [(Direction, Int, Int)]) -> ((Int, [(Direction, Int, Int)]), (Int, [(Direction, Int, Int)]), (Int, [(Direction, Int, Int)]), (Int, [(Direction, Int, Int)])) -> (Int, [(Direction, Int, Int)])
calc (v, c) ((_,n), (_,e), (_,s), (_,w)) = (v, c')
    where c' = merge [extend n South, extend e West, extend s North, extend w East] v c

extend :: [(Direction, Int, Int)] -> Direction -> [(Direction, Int, Int)]
extend [] _ = []
extend ((d, x, y):xs) d'
    | d == d' = if x < 3 then (d, x + 1, y) : extend xs d' else extend xs d'
    | d `opposite` d' = extend xs d'
    | otherwise = (d', 1, y) : extend xs d'

merge :: [[(Direction, Int, Int)]] -> Int -> [(Direction, Int, Int)] -> [(Direction, Int, Int)]
merge [] _ c = c
merge x v c = f $ c ++ map (\ (d, x, y) -> (d, x, y + v)) (concat x)
    where f l = reduce [minimumBy' (\ (_,_,x) (_,_,y) -> compare x y) $ filter (\ (d', x', y') -> d == d' && x == x') l | x <- [1..3], d <- [North, East, South, West]]



opposite :: Direction -> Direction -> Bool
opposite North South = True
opposite South North = True
opposite East West = True
opposite West East = True
opposite _ _ = False

reduce :: [Maybe a] -> [a]
reduce [] = []
reduce (Just x:xs) = x : reduce xs
reduce (Nothing:xs) = reduce xs

minimumBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy' _ [] = Nothing
minimumBy' f (x:xs) = Just $ minimumBy f (x:xs)

getValue :: Maybe (Direction, Int, Int) -> Maybe Int
getValue Nothing = Nothing
getValue (Just (_, _, x)) = Just x
