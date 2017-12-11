import Data.Char
import Data.List
import Data.Sequence as Seq
import System.Environment
import System.IO
import Lib.IOLib

input :: [Int]
input = [0,3,0,1,-3]

jump1 xs i acc
  | i < 0 || i >= len = acc
  | otherwise         = jump1 newL (i + curr) acc + 1
  where len  = Seq.length xs
        newL = Seq.update i (curr+1) xs
        curr = index xs i

jump2 xs i acc
  | i < 0 || i >= len = acc
  | otherwise         = jump2 newL (i + curr) acc + 1
  where len  = Seq.length xs
        newL = Seq.update i (curr+inc) xs
        inc  = if curr >= 3 then -1 else 1 
        curr = index xs i

part1 = jump1
part2 = jump2

main = do
  lines <- readLinesToStringList
  let inp = (Seq.fromList . map (read :: String->Int)) lines
  print $ part1 inp 0 0
  print $ part2 inp 0 0
