import           Data.List
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Lib.IOLib

testInput = ["0 <-> 2", "1 <-> 1", "2 <-> 0, 3, 4", "3 <-> 2, 4", "4 <-> 2, 3, 6", "5 <-> 6", "6 <-> 4, 5"]

readInput x = (key, vals)
    where [keys, valss] = splitOn " <-> " x
          key           = (read::String->Int) keys
          vals          = (map (read :: String->Int) . splitOn ", ") valss

inputsToMap :: [String] -> Map.Map Int [Int]
inputsToMap = Map.fromList . map readInput


getConnections :: Map.Map Int [Int] -> [Int] -> Int -> [Int]
getConnections m visited n = n:connects ++ concatMap (getConnections m newVis) connects
    where connects = (m Map.! n) \\ visited
          newVis   = nub $ n:visited

part1 xs = 1 + (length . nub $ getConnections m [] 0)
    where m = inputsToMap xs

part2 xs = length . nub $ map (nub . sort . getConnections m []) [0..len]
  where len = length xs - 1
        m   = inputsToMap xs

main = do
  lines <- readLinesToStringList
  print $ part1 lines
  print $ part2 lines
