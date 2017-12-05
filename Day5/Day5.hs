import Data.Char
import Data.List
import System.Environment
import System.IO
import Lib.IOLib

input :: [Int]
input = [0,3,0,1,-3]

jump xs i acc
  | i < 0 || i >= len = acc
  | otherwise         = jump newL (i + (xs !! i)) acc + 1
  where len  = length xs
        newL = take i xs ++ [(xs !! i) + 1] ++ drop (i+1) xs

main = do
  lines <- readLinesToStringList
  let inp = map (read :: String->Int) lines
  print $ jump inp 0 0
