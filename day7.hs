import Data.List

main = do
    contents <- readFile "inputs/day7.txt"
    let hands = sort $ map value $ map readLine $ lines contents
    
    print $ calc hands $ 1

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq)

type Hand = (Card, Card, Card, Card, Card)

type Value = (Bool, Bool, Bool, Bool, Bool, Int, Int, Int, Int, Int)

value :: (Hand, Int) -> (Value, Int)
value ((a,b,c,d,e), v) = ((five, four, three, twopair, pair, fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e), v)
    where l = [a,b,c,d,e]
          five = 5 == maxCount l
          four = 4 == maxCount l
          three = 3 == maxCount l
          twopair = (length $ filter (>= 2) $ map (count l) [Two .. Ace]) == 2
          pair = 2 <= maxCount l


readLine :: String -> (Hand, Int)
readLine (a:b:c:d:e:' ':xs) = ((readChar a, readChar b, readChar c, readChar d, readChar e), read xs)

readChar :: Char -> Card
readChar '2' = Two
readChar '3' = Three
readChar '4' = Four
readChar '5' = Five
readChar '6' = Six
readChar '7' = Seven
readChar '8' = Eight
readChar '9' = Nine
readChar 'T' = Ten
readChar 'J' = Jack
readChar 'Q' = Queen
readChar 'K' = King
readChar 'A' = Ace

count :: [Card] -> Card -> Int
count [] _ = 0
count (x:xs) y
    | x == y = 1 + count xs y
    | otherwise = count xs y

maxCount :: [Card] -> Int
maxCount x = maximum $ map (count x) $ [Two .. Ace]

calc :: [(Value, Int)] -> Int -> Int
calc [] _ = 0
calc ((_,x):xs) v = v * x + (calc xs (v+1))
