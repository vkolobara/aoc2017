import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Lib.IOLib
import qualified Control.Exception as Exc


testInput = 
  ["pbga (66)", 
  "xhth (57)", 
  "ebii (61)", 
  "havc (66)", 
  "ktlj (57)",
  "fwft (72) -> ktlj, cntj, xhth",
  "qoyq (66)",
  "padx (45) -> pbga, havc, qoyq",
  "tknk (41) -> ugml, padx, fwft",
  "jptl (61)",
  "ugml (68) -> gyxo, ebii, jptl",
  "gyxo (61)",
  "cntj (57)"]


data Program = Program 
  { name :: String
  , weight :: Int
  , discs :: [String] } deriving Show

mapStringToProgram :: String -> Program
mapStringToProgram s = prog
  where ss      = splitOn " -> " s
        [p, sd] = if (length ss == 1) then ss ++ [[]] else ss
        [n, w]  = words $ p
        ssd     = splitOn ", " sd
        ds      = if head ssd == "" then [] else ssd
        prog    = Program n (read w :: Int) ds

programs :: [String] -> [Program]
programs = map mapStringToProgram

programsToMap :: [Program] -> Map.Map String Program 
programsToMap xs = Map.fromList keyvals
  where keyvals = map (\p -> (name p, p)) xs 

findBottomProgram :: [Program] -> String
findBottomProgram ps = (head . Set.toList) (names Set.\\ ds)
  where names = (Set.fromList . map name) ps
        ds    = (Set.fromList . concatMap discs) ps

part1 :: [String] -> String
part1 = findBottomProgram . programs

checkTower :: Map.Map String Program -> String -> Int
checkTower m "" = 0
checkTower m bot 
  | (length . nub) ws <= 1 = sum ws
  | otherwise              = errorWithoutStackTrace $ show (errw + diff)
  where prog     = m Map.! bot
        w        = weight prog
        ds       = discs prog
        ps       = map (m Map.!) ds 
        ws       = map (\p -> weight p + (checkTower m $ name p)) ps
        (i,diff) = findDistinctIndex ws
        errw     = weight $ ps !! i

findDistinctIndex :: [Int] -> (Int, Int)
findDistinctIndex xs 
  | c1 < c2   = (findInd v1 xs, v2-v1)
  | otherwise = (findInd v2 xs, v1-v2)
  where [v1, v2] = nub xs
        [c1, c2] = [elemCount v1 xs, elemCount v2 xs]

findInd :: Int -> [Int] -> Int
findInd el xs = head [index | (index, e) <- zip [0..] xs, e == el]

elemCount :: Int -> [Int] -> Int
elemCount el = length . filter (==el)

part2 xs = checkTower m bot
  where bot = findBottomProgram ps
        ps  = programs xs
        m   = programsToMap ps 

part2Prog ls = Exc.catch (print $ part2 ls) handler
    where handler :: Exc.ErrorCall -> IO()
          handler e = putStrLn $ Exc.displayException e

main = do
  ls <- readLinesToStringList
  print $ part1 ls
  part2Prog ls