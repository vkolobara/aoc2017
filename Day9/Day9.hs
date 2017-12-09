import           Data.List
import           Lib.IOLib

testInput :: String
testInput = "{{{},{},{{}}}}"

data State = Open | Garbage | Ignore

groupScores :: String -> [Int]
groupScores = scores' (1,Open)
  where scores' (k,Open)     ('{':xs) = k:scores' (k+1, Open) xs
        scores' (k,Open)     ('}':xs) = scores'   (k-1, Open) xs
        scores' (k,Open)     ('<':xs) = scores'   (k,   Garbage) xs
        scores' (k,Garbage)  ('>':xs) = scores'   (k,   Open) xs
        scores' (k,Garbage)  ('!':xs) = scores'   (k,   Ignore) xs
        scores' (k,Ignore)   (_:xs)   = scores'   (k,   Garbage) xs
        scores' acc          (_:xs)   = scores'   acc            xs
        scores' _             _       = []

part1 :: String -> Int
part1 = sum . groupScores

countGarbage :: String -> Int
countGarbage = garbage' Open
  where garbage' Open    ('<':xs) = garbage' Garbage xs
        garbage' Garbage ('>':xs) = garbage' Open    xs
        garbage' Garbage ('!':xs) = garbage' Ignore xs
        garbage' Ignore  (_:xs)   = garbage' Garbage xs
        garbage' Garbage (_:xs)   = 1 + garbage' Garbage xs
        garbage' Open    (_:xs)   = garbage' Open xs
        garbage' _       _        = 0

part2 :: String -> Int
part2 = countGarbage

main = do
    s <- readLinesToStringList
    print $ (part1 . head) s
    print $ (part2 . head) s
