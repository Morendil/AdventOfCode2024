module Day10 where

import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Data.List (nub)

type Grid = M.Map (Int,Int) Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

trailStep :: Grid -> [(Int,Int)] -> [(Int,Int)]
trailStep grid = concatMap next
    where next (x,y) = filter rises (neighbors (x,y))
                        where rises p = M.findWithDefault '.' p grid == succ (fromJust $ M.lookup (x,y) grid)

trailFrom :: Grid -> (Int, Int) -> [[(Int,Int)]]
trailFrom grid start = take 10 $ iterate (trailStep grid) [start]

partOne :: Grid -> Int
partOne grid = sum $ map (length . nub . last . trailFrom grid) starts
    where starts = M.keys $ M.filter (=='0') grid

partTwo :: Grid -> Int
partTwo grid = sum $ map (length . last . trailFrom grid) starts
    where starts = M.keys $ M.filter (=='0') grid

main = do
    grid <- asMap . lines <$> readFile "day10.txt"
    print $ partOne grid
    print $ partTwo grid