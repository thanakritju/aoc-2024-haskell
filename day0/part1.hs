import System.IO

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  putStrLn $ "part 1 answer: " ++ show "0"