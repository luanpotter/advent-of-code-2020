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

validLine :: Line -> Bool
validLine line = let
  count = countChars (letter line) (pass line)
  in (count >= minL line) && (count <= maxL line)

countChars :: Char -> [Char] -> Int
countChars char str = length (filter (==char) str)

process :: String -> Int
process input = let
  lines = parseList input
  valids = filter validLine lines
  in length valids
