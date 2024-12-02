import System.IO

parseLine :: String -> [Int]
parseLine str = map read (words str)

differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs

isReportSafe :: [Int] -> Bool
isReportSafe xs = allIncreasing xs || allDecreasing xs

allIncreasing :: [Int] -> Bool
allIncreasing xs = all (\x -> x == 1 || x == 2 || x == 3) (differences xs)

allDecreasing :: [Int] -> Bool
allDecreasing xs = all (\x -> x == -1 || x == -2 || x == -3) (differences xs)

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  let inputs = map parseLine (lines content)

  let safes = map isReportSafe inputs

  putStrLn $ "part 1 answer: " ++ show (length (filter id safes))