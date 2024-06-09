import Data.List (sortOn, group, sort)

frequency :: Int -> String -> [[(Char, Int)]]
frequency n ct =
  let emptyFrequencyList = replicate n []
      charFrequencyList = zipWith (\i c -> (i, c)) [1..] ct
      frequencyLists = foldl (\lists (i, c) -> updateFrequencyList i c n lists) emptyFrequencyList charFrequencyList
  in map (sortOn (\(c, freq) -> (-freq, c)) . filter (\(c, freq) -> freq > 0)) frequencyLists

updateFrequencyList :: Int -> Char -> Int -> [[(Char, Int)]] -> [[(Char, Int)]]
updateFrequencyList _ _ _ [] = []
updateFrequencyList i c n (lst:lists)
  | i `mod` n == 1 = freqListInsert lst c : lists
  | otherwise = lst : updateFrequencyList i c (n-1) lists

freqListInsert :: [(Char, Int)] -> Char -> [(Char, Int)]
freqListInsert [] c = [(c, 1)]
freqListInsert ((char, freq):rest) c
  | char == c = (char, freq + 1) : rest
  | otherwise = (char, freq) : freqListInsert rest c
