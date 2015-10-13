import Data.List
import Data.Maybe
import qualified Data.Map as M

possibilities :: String -> [String] -> [String]
possibilities letters dictionary = filter (possibleWord letters) dictionary

possibleWord :: String -> String -> Bool
possibleWord xs [] = True
possibleWord xs (y:ys) = if y `elem` xs
  then possibleWord (delete y xs) ys
  else False

scores :: [String] -> [(String, Int)]
scores xs = map (\x -> (x, score x)) xs

score :: String -> Int
score = foldr (\tile acc -> acc + tileScore tile) 0
  where tileScore t = fromJust $ M.lookup t tiles


tiles = M.fromList [('a',1),('b',3),('c',3),('d',2)
                        ,('e',1),('f',4),('g',2),('h',4)
                        ,('i',1),('j',8),('k',5),('l',1)
                        ,('m',3),('n',1),('o',1),('p',3)
                        ,('q',10),('r',1),('s',1),('t',1)
                        ,('u',1),('v',4),('w',4),('y',4)
                        ,('x',8),('z',10)]

main = do
  dict <- readFile "./dictionary.csv"
  putStrLn "Input your scrabble letters:"
  tiles <- getLine
  putStrLn $ show 
    . sortBy comparator
    . scores
    . possibilities tiles $ map init $ lines dict
  where comparator a b = if snd a > snd b then LT else GT
