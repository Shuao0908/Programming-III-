import Data.List (sort, group, sortOn)

frequency :: Int -> String -> [[(Char, Int)]]
frequency n ct =
  let groupedCharacters = groupByPosition n ct
      frequencyLists = map (characterFrequencies . sort) groupedCharacters
  in frequencyLists

groupByPosition :: Int -> String -> [[Char]]
groupByPosition n ct =[[ct !! i | i <- [pos, pos + n..length ct - 1]] | pos <- [0..n-1]]

characterFrequencies :: String -> [(Char, Int)]
characterFrequencies ct =
  [(char, count) | char <- ['A'..'Z'], let count = length (filter (== char) ct), count > 0]

main :: IO ()
main = do
  let keyLength = 3
      ciphertext = "HELLO WORLD"
      result = frequency keyLength ciphertext
  print result

