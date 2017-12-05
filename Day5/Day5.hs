import Data.Char
import Data.List
import System.Environment
import System.IO
import Lib.IOLib

input :: [Int]
input = [0,3,0,1,-3]

jump1 xs i acc
  | i < 0 || i >= len = acc
  | otherwise         = jump1 newL (i + curr) acc + 1
  where len  = length xs
        newL = take i xs ++ [curr + 1] ++ drop (i+1) xs
        curr = xs !! i

jump2 xs i acc
  | i < 0 || i >= len = acc
  | otherwise         = jump2 newL (i + curr) acc + 1
  where len  = length xs
        newL = take i xs ++ [curr + inc] ++ drop (i+1) xs
        inc  = if curr >= 3 then -1 else 1 
        curr = xs !! i

part1 = jump1
part2 = jump2

main = do
  lines <- readLinesToStringList
  let inp = map (read :: String->Int) lines
--  print $ part1 inp 0 0
  print $ part2 inp 0 0
