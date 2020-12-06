import System.Environment
import Data.List
import Data.Maybe

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  print (process content)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd as bs = [(a, b) | a <- as, b <- bs]

parseList :: [Char] -> [Int]
parseList input = (map (\x -> read x :: Int) (lines input))

process :: String -> Int
process input = let
  numbers = parseList input
  pairs = cartProd numbers numbers
  pair = fromJust (find (\(x, y) -> x + y == 2020) pairs)
  in (fst pair) * (snd pair)
