import System.IO

countSubstring :: String -> String -> Int
countSubstring searchString inputString
  | length inputString < length searchString = 0
  | take (length searchString) inputString == searchString = 1 + countSubstring searchString (tail inputString)
  | otherwise = countSubstring searchString (tail inputString)

extractDiagonals :: [String] -> [String]
extractDiagonals rows = map extractDiagonal [0 .. totalDiagonals - 1]
  where
    n = length rows
    m = length (head rows)
    totalDiagonals = n + m - 1
    extractDiagonal d = [rows !! r !! c | r <- [0 .. n - 1], let c = d - r, c >= 0, c < m]

rotate :: [String] -> [String]
rotate rows = map extractVertical [0 .. n - 1]
  where
    n = length rows
    m = length (head rows)
    extractVertical c = [rows !! r !! c | r <- reverse [0 .. n - 1]]

countRow :: String -> Int
countRow s = countSubstring "XMAS" s + countSubstring "XMAS" (reverse s)

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName
  let inputs = lines content

  let count1 = sum (map countRow inputs)
  let count2 = sum (map countRow (rotate inputs))
  let count3 = sum (map countRow (extractDiagonals (rotate inputs)))
  let count4 = sum (map countRow (extractDiagonals inputs))

  putStrLn $ "part 1 answer: " ++ show (count1 + count2 + count3 + count4)