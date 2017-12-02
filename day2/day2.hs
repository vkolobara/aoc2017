import Data.List
import Data.Char
import System.Environment
import System.IO

calc :: (Int, Int) -> IO (Int, Int)
calc acc = do
  line <- getLine
  if null line
    then return (acc)
    else do
        --part1
        --calc (acc + (subMaxMin $ stringToIntList line))

        --part2
        calc (fst acc + (subMaxMin $ stringToIntList line), snd acc + (divideEvenly $ stringToIntList line))

main :: IO()
main = do
  res <- calc (0, 0)
  putStrLn $ show res

stringToIntList :: String -> [Int]
stringToIntList = map read . words

subMaxMin :: [Int] -> Int
subMaxMin xs = maximum xs - minimum xs

divideEvenly :: [Int] -> Int
divideEvenly xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]
