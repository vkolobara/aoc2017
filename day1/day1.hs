import System.IO
import System.Environment
import Data.List
import Data.Char

sumMatchingNextString :: String -> Int
sumMatchingNextString s = sumMatchingNext (digitToInt (last s):(map digitToInt s)) 0

sumMatchingNext :: [Int] -> Int -> Int
sumMatchingNext (x:xs@(y:_)) acc
  | x == y    = sumMatchingNext xs acc + y
  | otherwise = sumMatchingNext xs acc
sumMatchingNext _ acc = acc


main = do
  s <- getLine
  putStrLn $ show $ sumMatchingNextString s
