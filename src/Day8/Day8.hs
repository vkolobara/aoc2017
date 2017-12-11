import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Lib.IOLib

testInput =
  ["b inc 5 if a > 1",
  "a inc 1 if b < 5",
  "c dec -10 if a >= 1",
  "c inc -20 if c == 10"]

type Registers = Map String Integer
data Operator   = Inc String Integer | Dec String Integer deriving Show
data If        = Gt String Integer | Lt String Integer | Gte String Integer | Lte String Integer | Ne String Integer | Eq String Integer deriving Show
data Command      = Command Operator If deriving Show

showOperator :: Operator -> String
showOperator (Inc reg val) = unwords [reg, "inc", show val]
showOperator (Dec reg val) = unwords [reg, "dec", show val]

showIf :: If -> String
showIf (Gt n i)  = unwords [n, ">", show i]
showIf (Lt n i)  = unwords [n, "<", show i]
showIf (Gte n i) = unwords [n, ">=", show i]
showIf (Lte n i) = unwords [n, "<=", show i]
showIf (Ne n i)  = unwords [n, "!=", show i]
showIf (Eq n i)  = unwords [n, "==", show i]

showCommand (Command c cif) = intercalate " if " [showOperator c, showIf cif]

parseOperator :: String -> Operator
parseOperator line
  | t == "inc" = Inc reg val
  | t == "dec" = Dec reg val
  | otherwise  = error "Unknown command"
  where [reg,t,v] = words line
        val       = read v :: Integer

parseIf :: String -> If
parseIf line
  | t == ">"  = Gt reg val
  | t == "<"  = Lt reg val
  | t == ">=" = Gte reg val
  | t == "<=" = Lte reg val
  | t == "!=" = Ne reg val
  | t == "==" = Eq reg val
  | otherwise = error "Unknown condition"
  where [reg,t,v] = words line
        val       = read v :: Integer

parseCommand :: String -> Command
parseCommand line = Command com cif
  where [scom,scif] = splitOn " if " line
        com         = parseOperator scom
        cif         = parseIf      scif

getRegisterValue :: String -> Registers -> Integer
getRegisterValue = Map.findWithDefault 0

execOperator :: Operator -> Registers -> Registers
execOperator (Inc n i) regs = Map.insertWith (+) n i regs
execOperator (Dec n i) regs = Map.insert n (val-i) regs
  where val = getRegisterValue n regs

checkIf :: If -> Registers -> Bool
checkIf (Gt n i)  reg = getRegisterValue n reg >  i
checkIf (Lt n i)  reg = getRegisterValue n reg <  i
checkIf (Gte n i) reg = getRegisterValue n reg >= i
checkIf (Lte n i) reg = getRegisterValue n reg <= i
checkIf (Ne n i)  reg = getRegisterValue n reg /= i
checkIf (Eq n i)  reg = getRegisterValue n reg == i

execCommand :: Registers -> Command -> Registers
execCommand reg (Command c cif) = if checkIf cif reg then execOperator c reg else reg

parseAllCommands = map parseCommand

part1 = maximum . Map.elems . foldl execCommand Map.empty . parseAllCommands

part2 = maximum . concatMap Map.elems . scanl execCommand Map.empty . parseAllCommands

main = do
  s <- readLinesToStringList
  print $ part1 s
  print $ part2 s
