import Util (splitWhen)

main = do
    contents <- readFile "inputs/day15.txt"
    let line = head $ lines contents
        l =  splitWhen (== ',') line

    print $ sum $ map calc l

calc :: String -> Int
calc [] = 0
calc x = f x 0
    where f [] n = n
          f (x:xs) n = f xs (((n + fromEnum x) * 17) `mod` 256)