main = do
    let races = [(42, 308), (89, 1170), (91, 1291), (89, 1467)]
    print $ product $ map calc races

calc :: (Int, Int) -> Int
calc (t, d) = length $ filter (\ x -> x * (t - x) > d) [0..t]