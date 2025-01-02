import Util

main = do
    contents <- readFile "inputs/day5.txt"
    let l = lines contents
        seeds :: [Int] = map read $ splitWhen (== ' ') $ drop 7 $ head l
        seedRanges = seedRange seeds
    
    print $ calcAll (reverse $ calcMaps $ drop 2 l) seedRanges 0

calcMaps :: [String] -> [[(Int, Int, Int)]]
calcMaps [] = []
calcMaps l = f (tail m) : calcMaps ms
    where (m,ms) = splitFirst (== "") l
          f [] = []
          f (s:ss) = (x, y, z) : f ss
              where [x, y, z] = map read $ splitWhen (== ' ') s

seedRange :: [Int] -> [(Int, Int)]
seedRange [] = []
seedRange (x:y:xs) = (x, y) : seedRange xs

revMap :: [(Int, Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
revMap [] (min, range) = Nothing
revMap ((x, y, z):xs) (m, range)
    | m >= x && m < x + z = Just (m - x + y, min range (x + z - m))
    | otherwise = revMap xs (m, range)

revValue :: [(Int, Int, Int)] -> Int -> Int
revValue [] x = x
revValue ((x, y, z):xs) m
    | m >= x && m < x + z = y + (m - x)
    | otherwise = revValue xs m

forwardMap :: [(Int, Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
forwardMap [] (m, range) = Just (m, range)
forwardMap ((x, y, z):xs) (m, range)
    | m >= y && m < y + z = Nothing
    | y >= m && y < m + range = forwardMap xs (m, y - m)
    | otherwise = forwardMap xs (m, range)

-- takes a list of maps, a list of ranges, the starting values and the current value
calc :: [[(Int, Int, Int)]] -> [(Int, Int)] -> (Int, Int) -> Int -> Either Int Int
calc [] [] (min, range) start = Left (start + range)
calc [] ((seedStart, seedRange):rs) (min, r) start
    | min >= seedStart && min < seedStart + seedRange = Right start
    | seedStart >= min && seedStart < min + r = Right (start + (seedStart - min))
    | otherwise = calc [] rs (min, r) start

calc (m:ms) r (min, range) start = case (rev, forward) of
    (Nothing, Nothing) -> Left (min + range)
    (Just (x, y), Nothing) -> calc ms r (x, y) start
    (Nothing, Just (x, y)) -> calc ms r (x, y) start
    (Just (x, y), Just (a, b)) -> eitherMin (calc ms r (x, y) start) (calc ms r (a, b) start)

    where rev = revMap m (min, range)
          forward = forwardMap m (min, range)


eitherMin :: Either Int Int -> Either Int Int -> Either Int Int
eitherMin (Left x) (Left y) = Left (min x y)
eitherMin (Left x) (Right y) = Right y
eitherMin (Right x) (Left y) = Right x
eitherMin (Right x) (Right y) = Right (min x y)

calcAll :: [[(Int, Int, Int)]] -> [(Int, Int)] -> Int -> Int
calcAll m r x = case calc m r (x, maxBound) x of
    Left y -> calcAll m r y
    Right y -> y