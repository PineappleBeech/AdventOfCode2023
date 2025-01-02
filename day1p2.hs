import Text.Read (readMaybe)
import System.Win32 (xBUTTON1)
main = do
    contents <- readFile "inputs/day1.txt"
    print $ sum $ map calc $ lines contents


calc :: String -> Int
calc x = 10 * calcFirst x + calcFirstReverse (reverse x)

calcFirst :: String -> Int
calcFirst x
    | any (matchStart x . show') [0..9] = matchNum x
    | head x `elem` ['a'..'z'] = calcFirst $ tail x
    | otherwise = read [head x] :: Int

matchStart :: String -> String -> Bool
matchStart [] _ = False
matchStart _ [] = True
matchStart (x:xs) (y:ys)
    | x == y = matchStart xs ys
    | otherwise = False

matchNum :: String -> Int
matchNum x = f x 0
    where
        f xs n = case readMaybe' $ take n xs of
            Just x -> x
            Nothing -> f xs (n + 1)

calcFirstReverse :: String -> Int
calcFirstReverse x
    | any (matchStart x . reverse . show') [0..9] = matchNumReverse x
    | head x `elem` ['a'..'z'] = calcFirstReverse $ tail x
    | otherwise = read [head x] :: Int

matchNumReverse :: String -> Int
matchNumReverse x = f x 0
    where
        f xs n = case readMaybe' $ reverse $ take n xs of
            Just x -> x
            Nothing -> f xs (n + 1)

show' :: Int -> String
show' 0 = "zero"
show' 1 = "one"
show' 2 = "two"
show' 3 = "three"
show' 4 = "four"
show' 5 = "five"
show' 6 = "six"
show' 7 = "seven"
show' 8 = "eight"
show' 9 = "nine"

readMaybe' :: String -> Maybe Int
readMaybe' "zero" = Just 0
readMaybe' "one" = Just 1
readMaybe' "two" = Just 2
readMaybe' "three" = Just 3
readMaybe' "four" = Just 4
readMaybe' "five" = Just 5
readMaybe' "six" = Just 6
readMaybe' "seven" = Just 7
readMaybe' "eight" = Just 8
readMaybe' "nine" = Just 9
readMaybe' _ = Nothing
