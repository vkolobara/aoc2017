import           Data.List
import           Data.List.Split (splitOn)
import qualified Data.Map        as Map
import           Lib.IOLib

testInput = ["0: 3", "1: 2", "4: 4", "6: 4"]

inputToFirewall s = (d,r)
    where [d, r] = map (read :: String->Int) . splitOn ": " $ s

convertInputs = map inputToFirewall

caught val (d,r)= (d+val) `mod` (2*(r-1)) == 0

firewallPunishment :: [(Int,Int)] -> Int
firewallPunishment = foldl f 0
  where f acc (d,r) = if caught 0 (d,r) then d*r + acc else acc

part1 = firewallPunishment

isCaught :: [(Int,Int)] -> Int -> Bool
isCaught xs d = any (caught d) xs

part2 :: [(Int, Int)] -> Maybe Int
part2 xs = findIndex not $ map (isCaught xs) [0..]


main = do
    ss <- readLinesToStringList
    let xs = convertInputs ss
    print $ part1 xs
    print $ part2 xs
