import Data.List (transpose, span, elemIndices)
import Prelude hiding (cycle)
main = do
    contents <- readFile "inputs/day14.txt"
    let rocks = transpose $ lines contents
        l = iterate cycle rocks
        [a, b, c] = take 3 $ elemIndices 107949 $ map value l
        diff = b - a
        i = a + (1000000000 - a) `mod` diff
    print [a, b, c, diff]
    print i
    print $ value' $ l !! i
calc :: String -> Int
calc [] = 0
calc ('#':xs) = calc xs
calc x = count l (length $ filter (== 'O') a) + calc b
    where (a, b) = span (/= '#') x
          l = length x

value :: [String] -> Int
value = sum . map calc

calc' :: String -> Int
calc' [] = 0
calc' ('#':xs) = calc' xs
calc' ('.':xs) = calc' xs
calc' ('O':xs) = 1 + length xs + calc' xs

value' :: [String] -> Int
value' = sum . map calc'

count :: Int ->  Int -> Int
count _ 0 = 0
count x y = x + count (x-1) (y-1)

tiltLine :: String -> String
tiltLine [] = []
tiltLine ('#':xs) = '#' : tiltLine xs
tiltLine x = replicate rocks 'O' ++ replicate notrocks '.' ++ tiltLine b
    where (a, b) = span (/= '#') x
          l = length x
          rocks = length $ filter (== 'O') a
          notrocks = length $ filter (== '.') a

tilt :: [String] -> [String]
tilt = map tiltLine

cycle :: [String] -> [String]
cycle = reverse . transpose . tilt . reverse . transpose . tilt . reverse . transpose . tilt . reverse . transpose . tilt