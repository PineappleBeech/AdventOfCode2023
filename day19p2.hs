import Util (splitWhen)
import Data.Map (Map)
import qualified Data.Map as Map

main = do
    contents <- readFile "inputs/day19.txt"
    let l = lines contents
        a = takeWhile (/= "") l
        flows = Map.fromList $ map readFlow a
        x = ((1, 4000), (1, 4000), (1, 4000), (1, 4000))

    print $ calc flows x "in"

    


calc :: Map String [String] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> String -> Int
calc _ ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) "A" = (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)
calc _ (x, m, a, s) "R" = 0
calc flows x name = f flow
    where flow = Map.findWithDefault [] name flows
          f [next] = calc flows x next
          f (op:xs) = case check op' val x of
                        Right (a, b) -> if b then calc flows a next else f xs
                        Left (a, b) -> calc flows a next + calc flows b name
            where op' = take 2 op
                  val = read $ takeWhile (/= ':') $ drop 2 op
                  next = tail $ dropWhile (/= ':') op
          

check :: String -> Int -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> Either (((Int, Int), (Int, Int), (Int, Int), (Int, Int)), ((Int, Int), (Int, Int), (Int, Int), (Int, Int))) (((Int, Int), (Int, Int), (Int, Int), (Int, Int)), Bool)
check "x>" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (x1 > val) == (x2 > val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), x1 > val) else Left (((val + 1, x2), (m1, m2), (a1, a2), (s1, s2)), ((x1, val), (m1, m2), (a1, a2), (s1, s2)))
check "x<" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (x1 < val) == (x2 < val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), x1 < val) else Left (((x1, val - 1), (m1, m2), (a1, a2), (s1, s2)), ((val, x2), (m1, m2), (a1, a2), (s1, s2)))
check "m>" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (m1 > val) == (m2 > val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), m1 > val) else Left (((x1, x2), (val + 1, m2), (a1, a2), (s1, s2)), ((x1, x2), (m1, val), (a1, a2), (s1, s2)))
check "m<" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (m1 < val) == (m2 < val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), m1 < val) else Left (((x1, x2), (m1, val - 1), (a1, a2), (s1, s2)), ((x1, x2), (val, m2), (a1, a2), (s1, s2)))
check "a>" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (a1 > val) == (a2 > val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), a1 > val) else Left (((x1, x2), (m1, m2), (val + 1, a2), (s1, s2)), ((x1, x2), (m1, m2), (a1, val), (s1, s2)))
check "a<" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (a1 < val) == (a2 < val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), a1 < val) else Left (((x1, x2), (m1, m2), (a1, val - 1), (s1, s2)), ((x1, x2), (m1, m2), (val, a2), (s1, s2)))
check "s>" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (s1 > val) == (s2 > val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), s1 > val) else Left (((x1, x2), (m1, m2), (a1, a2), (val + 1, s2)), ((x1, x2), (m1, m2), (a1, a2), (s1, val)))
check "s<" val ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if (s1 < val) == (s2 < val) then Right (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), s1 < val) else Left (((x1, x2), (m1, m2), (a1, a2), (s1, val - 1)), ((x1, x2), (m1, m2), (a1, a2), (val, s2)))





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
