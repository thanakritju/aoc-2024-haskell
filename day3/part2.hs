import System.IO
import Text.Regex.TDFA

parseLine :: String -> [String]
parseLine line = getAllTextMatches (line =~ "mul\\([0-9]{1,3}\\,[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)" :: AllTextMatches [] String)

filterLine :: [String] -> [String]
filterLine = go True
  where
    go _ [] = []
    go keepState (x : xs)
      | isDo x = go True xs
      | isDoNot x = go False xs
      | keepState = x : go keepState xs
      | otherwise = go keepState xs

isDo :: String -> Bool
isDo item = item == "do()"

isDoNot :: String -> Bool
isDoNot item = item == "don't()"

extractNumbers :: String -> [String]
extractNumbers item = getAllTextMatches (item =~ "[0-9]+" :: AllTextMatches [] String)

parseItem :: String -> Int
parseItem item = case extractNumbers item of
  [a, b] -> read a * read b
  _ -> error $ "Malformed mul(): " ++ item

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  let inputs = lines content

  let ans = (sum . map parseItem . filterLine . parseLine) content

  putStrLn $ "part 2 answer: " ++ show ans