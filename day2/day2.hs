import Data.List
import Data.Char
import System.Environment
import System.IO

stringToIntList :: String -> [Int]
stringToIntList = map read . words

subMaxMin :: [Int] -> Int
subMaxMin xs = maximum xs - minimum xs

divideEvenly :: [Int] -> Int
divideEvenly xs = head [y `div` x | (x:ys) <- init $ tails sorted, y <- ys, y `mod` x == 0]
  where sorted = sort xs

calc :: (Int, Int) -> IO (Int, Int)
calc acc = do
  line <- getLine
  if null line
    then return (acc)
    else do
        let lst = stringToIntList line
        calc (fst acc + (subMaxMin lst), snd acc + (divideEvenly lst))

main :: IO()
main = do
  res <- calc (0, 0)
  putStrLn $ show res
