import Util (splitWhen)
import Data.List (nubBy)

main = do
    contents <- readFile "inputs/day15.txt"
    let line = head $ lines contents
        l = map readOp $ splitWhen (== ',') line
        boxes = foldl applyOp (replicate 256 []) l
    print $ take 4 boxes
    print $ sum $ zipWith (*) [1..] $ map valueBox boxes

calc :: String -> Int
calc [] = 0
calc x = f x 0
    where f [] n = n
          f (x:xs) n = f xs (((n + fromEnum x) * 17) `mod` 256)

readOp :: String -> (String, Int)
readOp s
    | '=' `elem` s = (takeWhile (/= '=') s, read $ tail $ dropWhile (/= '=') s)
    | otherwise = (init s, 0)

applyOp :: [[(String, Int)]] -> (String, Int) -> [[(String, Int)]]
applyOp boxes lens@(boxid, val) = take box boxes ++ [newbox] ++ drop (box + 1) boxes
    where box = calc boxid
          oldbox = boxes !! box
          newbox = addlens oldbox lens

addlens :: [(String, Int)] -> (String, Int) -> [(String, Int)]
addlens [] lens
    | snd lens == 0 = []
    | otherwise = [lens]
addlens (x:xs) lens@(boxid, val)
    | boxid == fst x = if val == 0 then xs else (boxid, val) : xs
    | otherwise = x : addlens xs lens

valueBox :: [(String, Int)] -> Int
valueBox = sum . zipWith (*) [1..] . map snd