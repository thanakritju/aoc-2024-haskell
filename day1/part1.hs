import Data.List (sort)
import System.IO

parsePair :: String -> (Int, Int)
parsePair str = case map read (words str) of
  [a, b] -> (a, b)
  _ -> error "Input does not contain exactly two numbers"

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  let pairs = map parsePair (lines content)

  let left = map fst pairs
  let right = map snd pairs

  let sortedLeft = sort left
  let sortedRight = sort right

  let diff = zipWith (\x y -> abs (x - y)) sortedLeft sortedRight

  putStrLn $ "part 1 answer: " ++ show (sum diff)