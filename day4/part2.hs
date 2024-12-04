import System.IO

processRows :: [[Char]] -> Int
processRows arr =
  sum
    [ isXMAS arr r c | r <- [0 .. length arr - 1], c <- [0 .. length (arr !! r) - 1]
    ]

isXMAS :: [[Char]] -> Int -> Int -> Int
isXMAS arr r c
  | r >= length arr - 1 || r <= 0 = 0
  | c >= length (head arr) - 1 || c <= 0 = 0
  | (arr !! r !! c) /= 'A' = 0
  | (((arr !! (r - 1) !! (c - 1)) == 'M' && (arr !! (r + 1) !! (c + 1)) == 'S') || ((arr !! (r - 1) !! (c - 1)) == 'S' && (arr !! (r + 1) !! (c + 1)) == 'M')) && (((arr !! (r - 1) !! (c + 1)) == 'M' && (arr !! (r + 1) !! (c - 1)) == 'S') || ((arr !! (r - 1) !! (c + 1)) == 'S' && (arr !! (r + 1) !! (c - 1)) == 'M')) = 1
  | otherwise = 0

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  let inputs = lines content

  putStrLn $ "part 2 answer: " ++ show (processRows inputs)