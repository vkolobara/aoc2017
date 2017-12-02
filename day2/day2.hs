import Data.List
import Data.Char
import System.Environment
import System.IO

calc :: Int -> IO Int
calc acc = do
  line <- getLine
  if null line
    then return (acc)
    else do
        calc (acc + (subMaxMin $ stringToIntList line))

main :: IO()
main = do
  res <- calc 0
  putStrLn $ show res

stringToIntList :: String -> [Int]
stringToIntList = map read . words

subMaxMin :: [Int] -> Int
subMaxMin xs = maximum xs - minimum xs


