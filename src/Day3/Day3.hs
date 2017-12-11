import Data.List
import Data.Char
import Data.Ord
import Data.Map
import System.Environment
import System.IO

firstLargerSquare :: Int -> Int
firstLargerSquare num = findSquare 1
  where findSquare n | n*n >= num = n
                     | otherwise  = findSquare (n+2)

centers :: Int -> [Int]
centers n = [n*n - i*dst | i <- [1,3,5,7]]
  where dst = n `div` 2

distFromCenters :: Int -> [Int] -> [Int]
distFromCenters num cents = [ abs $ num-c | c <- cents]

distance :: Int -> Int
distance num = d + n `div` 2
  where n  = firstLargerSquare num
        cs = centers n
        d  = minimum $ distFromCenters num cs

part1 :: Int -> Int
part1 = distance

main = do
  s <- getLine
  let num = read s :: Int
  print $ part1 num



