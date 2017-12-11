import           Data.List
import           Data.List.Split (splitOn)
import           Lib.IOLib

type Coord     = (Int, Int)
data Direction = N | NE | SE | S | SW | NW | C deriving Show

createDir :: String -> Direction
createDir "n"  = N
createDir "ne" = NE
createDir "se" = SE
createDir "s"  = S
createDir "sw" = SW
createDir "nw" = NW

move :: Coord -> Direction -> Coord
move (x,y) N  = (x, y+2)
move (x,y) NE = (x+1, y+1)
move (x,y) SE = (x+1, y-1)
move (x,y) S  = (x, y-2)
move (x,y) SW = (x-1, y-1)
move (x,y) NW = (x-1, y+1)

distance :: Coord -> Int
distance (0, y) = y `div` 2
distance (x, 0) = x `div` 2
distance (x, y) = 1 + distance (x-1, y-1)

absolute :: Coord -> Coord
absolute (x,y) = (abs x, abs y)

part1 :: String -> Int
part1 = distance . absolute . foldl move (0,0) . map createDir . splitOn ","

part2 :: String -> Int
part2 = maximum . map (distance . absolute) . scanl move (0,0) . map createDir . splitOn ","

main = do
    s <- getLine
    print $ part1 s
    print $ part2 s
