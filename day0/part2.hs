import System.IO

main :: IO ()
main = do
  let fileName = "input.txt"

  content <- readFile fileName

  putStrLn $ "part 2 answer: " ++ show "0"