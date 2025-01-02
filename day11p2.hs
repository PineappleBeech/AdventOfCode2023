import Data.List (transpose)
main = do
    contents <- readFile "inputs/day11.txt"
    let space = lines contents
        space' = transpose space
        n = sum $ map (length . filter (== '#')) space

    print n
    print (calc space n 0 + calc space' n 0)

calc :: [[Char]] -> Int -> Int -> Int
calc [] _ _ = 0
calc (x:xs) n k
    | '#' `elem` x = n `choose` k + calc xs n (k + length (filter (== '#') x))
    | otherwise = 1000000 * (n `choose` k) + calc xs n k


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)


--choose :: Int -> Int -> Int
--choose n k = fact n `div` (fact k * fact (n-k))

--choose :: Int -> Int -> Int
--choose _ 0 = 1
--choose n k = (choose (n-1) (k-1) `div` k) * n

choose :: Int -> Int -> Int
choose n k = (n-k) * k

