import System.Environment
import Data.List
import Data.Maybe
import Text.Regex
import Debug.Trace

main = do
  args <- getArgs
  content <- readFile (head args)
  print (process content)

toInt :: [Char] -> Int
toInt str = read str :: Int

data Line = Line {
  minL :: Int,
  maxL :: Int,
  letter :: Char,
  pass :: [Char]
} deriving (Show)

parseList :: [Char] -> [Line]
parseList input = map parseLine (lines input)

parseLine :: [Char] -> Line
parseLine line = let
  tokens = splitRegex (mkRegex "[-,: ]") line
  minL = head tokens
  maxL = tokens !! 1
  char = tokens !! 2
  pass = tokens !! 4
  in Line (toInt minL) (toInt maxL) (head char) pass

validLine1 :: Line -> Bool
validLine1 line = let
  count = countChars (letter line) (pass line)
  in (count >= minL line) && (count <= maxL line)

validLine2 :: Line -> Bool
validLine2 line = let
  str = pass line
  char = letter line
  first = matchChar str (minL line - 1) char
  second = matchChar str (maxL line - 1) char
  in xor first second

matchChar :: [Char] -> Int -> Char -> Bool
matchChar str idx char
  | idx < 0 || idx >= length str = False
  | otherwise = str !! idx == char

countChars :: Char -> [Char] -> Int
countChars char str = length (filter (==char) str)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

process :: String -> Int
process input = let
  lines = parseList input
  valid = filter validLine2 lines
  in length valid
