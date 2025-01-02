main = do
    print $ calc (42899189, 308117012911467)

calc :: (Int, Int) -> Int
calc (t, d) = length $ filter (\ x -> x * (t - x) > d) [0..t]
