module Util where

splitFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitFirst _ [] = ([], [])
splitFirst p l
    | p $ head l = ([], tail l)
    | otherwise = (head l : fst a, snd a)
        where a = splitFirst p $ tail l

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p l
    | p $ head l = splitWhen p $ tail l
    | otherwise = splitWhen p l
        where splitWhen _ [] = []
              splitWhen p l = fst a : splitWhen p (snd a)
                where a = splitFirst p l

build :: ((Int, Int) -> a) -> (Int, Int) -> [[a]]
build f (y2, x2) = map (\y -> map (\x -> f (y, x)) [0..x2-1]) [0..y2-1]

group :: [a] -> [(a,a,a)]
group (x:y:z:xs) = (x,y,z) : group (y:z:xs)
group _ = []

calcRow :: (a -> (a, a, a, a) -> a) -> ([a], [a], [a]) -> [a]
calcRow f (_:_:[], _:_:[], _:_:[]) = []
calcRow f (x1:x2:x3:xs, y1:y2:y3:ys, z1:z2:z3:zs) = f y2 (x2, y3, z2, y1) : calcRow f (x2:x3:xs, y2:y3:ys, z2:z3:zs)

pad :: a -> [[a]] -> [[a]]
pad v area = map (\x -> v : x ++ [v]) (a : area ++ [a])
    where a = replicate (length (head area)) v

iterArea :: (a -> (a, a, a, a) -> a) -> a -> [[a]] -> [[a]]
iterArea f v area = map (calcRow f) $ group $ pad v area