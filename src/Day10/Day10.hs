import           Data.Bits       (xor)
import           Data.Char
import           Data.List
import           Data.List.Split (chunksOf, splitOn)
import           Numeric         (showHex)

doCycle :: [Int] -> [Int] -> ((Int, Int), [Int])
doCycle = doCycle' 0 0
doCycle' curr skip xs (l:ls) = doCycle' newPos (skip+1) newXs ls
  where newPos = (curr + skip + l) `mod` len
        len    = length xs
        rev    = (reverse . take l . drop curr . cycle $ xs) ++ (take (len-l) . drop (curr+l) . cycle $ xs)
        newXs  = rotate curr rev
doCycle' curr skip xs []     =  ((curr, skip), xs)

rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate n xs = rotate (n - 1) (last xs : init xs)

part1 :: [Int] -> [Int] -> Int
part1 xs ls = product $ take 2 $ snd $ doCycle xs ls

preprocess :: String -> [Int]
preprocess xs = map ord xs ++ [17, 31, 73, 47, 23]

postprocess :: [Int] -> String
postprocess = concatMap (intToHex . foldl xor 0) . chunksOf 16

doHash :: Int -> [Int] -> [Int] -> [Int]
doHash = doHash' 0 0
  where doHash' _ _ 0 xs _        = xs
        doHash' curr skip n xs ls = doHash' newCurr newSkip (n-1) newXs ls
            where ((newCurr, newSkip), newXs) = doCycle' curr skip xs ls

intToHex :: Int -> String
intToHex n
    | l < 2     = '0':s
    | otherwise = s
    where s = showHex n ""
          l = length s

part2 :: [Int] -> String -> String
part2 xs ls = postprocess $ doHash 64 xs (preprocess ls)

main = do
    s <- getLine
    let ls = map (read::String->Int) . splitOn "," $ s
    print $ part1 [0..255] ls
    print $ part2 [0..255] s
