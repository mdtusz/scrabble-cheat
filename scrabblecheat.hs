import Data.List

possibilities :: String -> [String] -> [String]
possibilities letters dictionary = filter (possibleWord letters) dictionary

possibleWord :: String -> String -> Bool
possibleWord xs [] = True
possibleWord xs (y:ys) = if y `elem` xs
  then possibleWord (delete y xs) ys
  else False

main = do
  dict <- readFile "./dictionary.csv"
  putStrLn "Input your scrabble letters:"
  tiles <- getLine
  putStrLn $ show . possibilities tiles $ map init $ lines dict
