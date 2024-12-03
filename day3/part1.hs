import System.IO
import Text.Regex.TDFA

parseLine :: String -> [String]
parseLine line = getAllTextMatches (line =~ "mul\\([0-9]{1,3}\\,[0-9]{1,3}\\)" :: AllTextMatches [] String)

extractNumbers :: String -> [String]
extractNumbers line = getAllTextMatches (line =~ "[0-9]+" :: AllTextMatches [] String)

parseItem :: String -> Int
parseItem line = read (head (extractNumbers line)) * read (extractNumbers line !! 1)

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  let ans = ((sum . map parseItem) . parseLine) content

  putStrLn $ "part 1 answer: " ++ show ans