import Data.List
import Data.Char
import System.Environment
import System.IO
import Lib.IOLib

stringToIntList :: String -> [Int]
stringToIntList = map read . words

subMaxMin :: [Int] -> Int
subMaxMin [] = 0
subMaxMin xs = maximum xs - minimum xs

divideEvenly :: [Int] -> Int
divideEvenly [] = 0
divideEvenly xs = head [y `div` x | (x:ys) <- init $ tails sorted, y <- ys, y `mod` x == 0]
  where sorted = sort xs

part1 :: [[Int]] -> Int
part1 = sum . map subMaxMin

part2 :: [[Int]] -> Int
part2 = sum . map divideEvenly

main :: IO()
main = do
  input <- readLinesToStringList
  let inputs = map stringToIntList input
  print $ part1 inputs
  print $ part2 inputs
