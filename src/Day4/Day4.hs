import Data.Char
import Data.List
import System.Environment
import System.IO
import Lib.IOLib
import Data.Map (fromListWith, toList)

checkIfValid1 xs = ws == nub ws
  where ws = words xs

checkIfValid2 xs = ws == nub ws
  where ws = (map sort . words) xs

part1 = length . filter (==True) . map checkIfValid1
part2 = length . filter (==True) . map checkIfValid2

main = do
  lines <- readLinesToStringList
  print $ part1 lines
  print $ part2 lines