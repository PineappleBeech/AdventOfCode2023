import Data.Map.Strict (fromList, Map, (!))
import GHC.Data.ShortText (ShortText(contents))

main = do
    contents <- readFile "inputs/day8.txt"
    let moves = cycle $ head $ lines contents
        network = fromList $ map readLine $ drop 2 $ lines contents

    print $ calc network moves "AAA"

readLine :: String -> (String, (String, String))
readLine s = (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))

calc :: Map String (String, String) -> [Char] -> String -> Int
calc _ _ "ZZZ" = 0
calc m ('L':xs) t = 1 + (calc m xs $ fst $ m ! t)
calc m ('R':xs) t = 1 + (calc m xs $ snd $ m ! t)
