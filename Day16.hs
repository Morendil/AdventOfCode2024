module Day16 where

import Algorithm.Search

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.List (nub)

import Debug.Trace

type Pos = (Int,Int)
type Dir = (Int,Int)
data Move = Step | Turn deriving (Eq, Show, Ord)
type Step = (Move, Pos, Dir)
type Grid = M.Map Pos Char

type Path = (Bool, Int, S.Set Step, Step)

add :: Pos -> Dir -> Pos
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

times :: Int -> Dir -> Dir
times n (x,y) = (n*x,n*y)

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

nextMoves :: Grid -> Step -> [Step]
nextMoves grid (Turn, pos,dir) = [(Step, add pos dir, dir) | M.findWithDefault '.' pos grid /= '#']
nextMoves grid (Step, pos,dir) = filter valid [(Step, add pos dir,dir),(Turn, pos,left),(Turn, pos, right)]
    where valid (Step,p,dir) = M.findWithDefault '.' p grid /= '#'
          valid (Turn,p,dir) = M.findWithDefault '.' (add p dir) grid /= '#'
          left = swap dir
          right = times (-1) (swap dir)

cost :: Step -> Step -> Int
cost _ (Turn,_,_) = 1000
cost _ (Step,_,_) = 1

getPos :: Step -> Pos
getPos (_,pos,_) = pos

partOne :: Grid -> Int
partOne maze = fst $ fromJust $ dijkstra (nextMoves maze) cost ((==) end.getPos) start
    where start = (Step, head $ M.keys $ M.filter ('S' ==) maze, (1,0))
          end = head $ M.keys $ M.filter ('E' ==) maze

main = do
    maze <- M.filter ('.' /=) . asMap . lines <$> readFile "day16.txt"
    print $ partOne maze
