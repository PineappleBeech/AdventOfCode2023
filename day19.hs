import Util (splitWhen)
import Data.Map (Map)
import qualified Data.Map as Map

main = do
    contents <- readFile "inputs/day19.txt"
    let l = lines contents
        a = takeWhile (/= "") l
        b = tail $ dropWhile (/= "") l
        parts = map readPart b
        flows = Map.fromList $ map readFlow a

    print parts
    print $ sum $ map (\ x -> calc flows x "in") parts


calc :: Map String [String] -> (Int, Int, Int, Int) -> String -> Int
calc _ (x, m, a, s) "A" = x + m + a + s
calc _ (x, m, a, s) "R" = 0
calc flows (x, m, a, s) name = f flow
    where flow = Map.findWithDefault [] name flows
          f [next] = calc flows (x, m, a, s) next
          f (op:xs) = if check op' val (x, m, a, s) then calc flows (x, m, a, s) next else f xs
            where op' = take 2 op
                  val = read $ takeWhile (/= ':') $ drop 2 op
                  next = tail $ dropWhile (/= ':') op
          

check :: String -> Int -> (Int, Int, Int, Int) -> Bool
check "x>" val (x, m, a, s) = x > val
check "x<" val (x, m, a, s) = x < val
check "m>" val (x, m, a, s) = m > val
check "m<" val (x, m, a, s) = m < val
check "a>" val (x, m, a, s) = a > val
check "a<" val (x, m, a, s) = a < val
check "s>" val (x, m, a, s) = s > val
check "s<" val (x, m, a, s) = s < val




readPart :: String -> (Int, Int, Int, Int)
readPart l = (x, m, a, s)
    where x = read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= 'x') l
          m = read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= 'm') l
          a = read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= 'a') l
          s = read $ takeWhile (/= '}') $ drop 2 $ dropWhile (/= 's') l

readFlow :: String -> (String, [String])
readFlow s = (name, ops)
    where name = takeWhile (/= '{') s
          ops = splitWhen (== ',') $ takeWhile (/= '}') $ tail $ dropWhile (/= '{') s
