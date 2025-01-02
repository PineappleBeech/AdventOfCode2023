import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty, (:<|)), fromList, Seq((:|>)))
import Util (splitWhen)
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Foldable (toList)


main = do
    contents <- readFile "inputs/test20.txt"
    let gates = mconcat $ map readGate $ lines contents
        (a', b') = runN 1000 (Sum 0, Sum 0) gates $ makeState gates
        a = getSum $ fst a'
        b = getSum $ snd a'

    print gates
    print (a, b)
    print (a * b)
    print $ fmap (\ x -> (x, False, "broadcaster")) $ snd (gates ! "broadcaster")


readGate :: String -> Map String (Char, Seq String)
readGate s = Map.singleton name' (head name, destinations)
    where name = takeWhile (/= ' ') s
          destinations = fromList $ map (tail) $ splitWhen (== ',' ) $ tail $ dropWhile (/= '>') s
          name' = if name == "broadcaster" then "broadcaster" else tail name

run :: Map String (Char, Seq String) -> Map String (Either Bool (Map String Bool)) -> ((Sum Int, Sum Int), Map String (Either Bool (Map String Bool)))
run gates state = (acc <> acc', state')
    where queue = fmap (\ x -> (x, False, "broadcaster")) $ snd (gates ! "broadcaster")
          (acc, state') = step gates state queue
          acc' = (Sum (1 + length (snd (gates ! "broadcaster"))), Sum 0)


step :: Map String (Char, Seq String) -> Map String (Either Bool (Map String Bool)) -> Seq (String, Bool, String) -> ((Sum Int, Sum Int), Map String (Either Bool (Map String Bool)))
step _ state Empty = ((Sum 0, Sum 0), state)
step gates state ((gate, high, previous) :<| queue)
--step gates state (queue :|> (gate, high, previous))
    | op == 'x' = step gates state queue
    | op == '%' && (not high) = 
                                let (acc, state') = step gates (Map.adjust (Left . notL) gate state) (queue <> fmap (\x -> (x, notL (state ! gate), gate)) (snd $ gates ! gate))
                                    count = length $ snd $ gates ! gate
                                    acc' = case state ! gate of
                                        Left True -> (Sum count, Sum 0)
                                        Left False -> (Sum 0, Sum count)
                                in (acc <> acc', state')

    | op == '%' = step gates state queue
    | op == '&' = let newstate = Map.adjust (\ (Right x) -> Right $ Map.insert previous high x) gate state
                      out = not $ allR (newstate ! gate)
                      (acc, state') = step gates newstate (queue <> fmap (\x -> (x, out, gate)) (snd $ gates ! gate))
                      count = length $ state ! gate
                      acc' = if out then (Sum 0, Sum count) else (Sum count, Sum 0)
                  in (acc <> acc', state')
    | otherwise = error "not a gate"
    where op = case gates !? gate of
                Just x -> fst x
                Nothing -> 'x'

notL :: Either Bool a -> Bool
notL (Left b) = not b

allR :: Either a (Map b Bool) -> Bool
allR (Right m) = all id $ Map.elems m

makeState :: Map String (Char, Seq String) -> Map String (Either Bool (Map String Bool))
makeState gates = Map.mapMaybeWithKey f gates
    where f "broadcaster" _ = Nothing
          f gate (op, destinations) = Just $ case op of
            '%' -> Left False
            '&' -> Right $ Map.fromList $ map (\ (x, (_, _)) -> (x, False)) $ filter (\ (x, (_, dest)) -> gate `elem` dest) $ Map.toList gates

runN :: Int -> (Sum Int, Sum Int) -> Map String (Char, Seq String) -> Map String (Either Bool (Map String Bool)) -> ((Sum Int, Sum Int), Map String (Either Bool (Map String Bool)))
runN 0 acc _ state = (acc, state)
runN n acc gates state = runN (n - 1) (acc <> acc') gates state'
    where (acc', state') = run gates state
