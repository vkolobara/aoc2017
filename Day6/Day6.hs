import Data.Char
import Data.List (map, transpose, maximumBy, elemIndex, findIndex)
import Data.List.Split
import Data.Map (Map, insert, empty, member, (!))
import Data.Ord
import System.Environment
import System.IO

findMaxElIndex :: [Int] -> (Int, Int)
findMaxElIndex xs = (m, i)
    where m = maximum xs
          i = ((\(Just x) -> x) . findIndex (==m)) xs

doIteration xs = map sum . transpose $ chunkL
    where (m, i) = findMaxElIndex xs
          len    = length xs
          chunkL = chunksOf len newL
          newL   = xsRem ++ ((replicate (i+1) 0) ++ (take (m) $ repeat 1))
          xsRem  = take i xs ++ [(xs !! i) - m] ++ drop (i+1) xs

execute xs m acc 
    | member xs m = (acc, acc - (m ! xs))
    | otherwise           = execute nextStep (insert xs acc m) (acc+1)
        where nextStep = doIteration xs

day6 xs = execute xs empty 0

main = do
    s <- getLine
    let input   = (map (read :: String -> Int) . words) s
    let (p1,p2) = day6 input
    print $ p1
    print $ p2