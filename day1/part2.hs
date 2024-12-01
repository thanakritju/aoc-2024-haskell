import Data.List (sort)
import System.IO

parsePair :: String -> (Int, Int)
parsePair str = case map read (words str) of
  [a, b] -> (a, b)
  _ -> error "Input does not contain exactly two numbers"

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences x xs = length (filter (== x) xs)

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  let pairs = map parsePair (lines content)

  let left = map fst pairs
  let right = map snd pairs

  let each = map (\x -> countOccurrences x right * x) left

  putStrLn $ "part 2 answer: " ++ show (sum each)