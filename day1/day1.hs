import System.Environment
import Data.List
import Data.Maybe

main = do
  args <- getArgs
  content <- readFile (head args)
  print (process content 3)

cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (xs:rest) = [x:ys | x <- xs, ys <- cartProd rest]

parseList :: String -> [Int]
parseList input = map (\x -> read x :: Int) (lines input)

process :: String -> Int -> Int
process input num = let
  numbers = parseList input
  tuples = cartProd (replicate num numbers)
  tuple = fromJust (find (\x -> sum x == 2020) tuples)
  in product tuple
