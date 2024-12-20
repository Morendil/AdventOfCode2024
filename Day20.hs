module Day20 where

import Algorithm.Search
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)

type Pos = (Int,Int)
type Grid = M.Map Pos Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

skips :: (Int,Int) -> [(Int,Int)]
skips (x,y) = [(x-2,y),(x+2,y),(x,y-2),(x,y+2)]

path :: Grid -> [Pos]
path maze = start : fromJust (bfs nextMoves (end ==) start)
    where start = head $ M.keys $ M.filter ('S' ==) maze
          end = head $ M.keys $ M.filter ('E' ==) maze
          nextMoves = filter valid . neighbors
          valid pos = M.findWithDefault '.' pos maze /= '#'

cheatsFrom :: [Pos] -> Pos -> [(Int,(Pos,Pos))]
cheatsFrom path pos = filter ((<) 0.fst) $ map saves $ filter (`elem` path) $ skips pos
    where saves skip = (fromJust (elemIndex skip path) - fromJust (elemIndex pos path) - 2,(pos,skip))

partOne :: Grid -> Int
partOne maze = length $ concatMap (filter ((>= 100) . fst) . cheatsFrom route) route
    where route = path maze

main = do
    maze <- asMap . lines <$> readFile "day20.txt"
    print $ partOne maze