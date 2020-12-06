import System.Environment
import Data.List
import Data.Maybe
import Text.Regex
import Debug.Trace

main = do
  args <- getArgs
  content <- readFile (head args)
  print (process content)

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

filterInd :: (a -> Int -> Bool) -> [a] -> [a]
filterInd f l = map fst (filter (uncurry f) (zip l [0..]))

process :: String -> Int 
process input = let
  rows = lines input
  tests = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  results = map (uncurry (countSlopes rows)) tests
  in product results

countSlopes :: [String] -> Int -> Int -> Int
countSlopes rows slopeX slopeY = let
  skipH = mapInd (\row idx -> row !! ((slopeX * idx) `mod` length row)) rows
  skipV = filterInd (\row idx -> idx `mod` slopeY == 0) skipH
  trees = filter (=='#') skipV
  in length trees
