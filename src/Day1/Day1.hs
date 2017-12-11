import System.IO
import System.Environment
import Data.List
import Data.Char
import Lib.IOLib

stringToIntList = map digitToInt

sumMatchingNextN :: [Int] -> Int -> Int
sumMatchingNextN xs n = sum [xs !! i | i <- [0..len-1], xs !! i == lst !! (i+n)]
  where len = length xs
        lst = cycle xs

main = do
  s <- getLine
  let lst = stringToIntList s
  let half = length lst `div` 2
  print $ sumMatchingNextN lst 1 
  print $ sumMatchingNextN lst half
