import Data.List

possibilities :: String -> [String]
possibilities = concat . map permutations . subsequences

dictionary :: IO String
dictionary = readFile "./dictionary.csv"

main = do
  dict <- readFile "./dictionary.csv"
  putStrLn "Input your scrabble letters."
  letters <- getLine
  putStrLn "Thinking. This may take a while..."
  putStrLn $ show . intersect (map init $ lines dict) . possibilities $ letters