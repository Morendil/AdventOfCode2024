module Main where

import Algorithm.Search

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.List (nub)

type Pos = (Int,Int)
type Dir = (Int,Int)
data Move = Step | Turn deriving (Eq, Show, Ord)
type Step = (Move, Pos, Dir)
type Grid = M.Map Pos Char
type CostMap = M.Map Step Int

type Path = (Bool, Int, [Step], Step)

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

advance :: Grid -> Pos -> [Path] -> [Path]
advance grid goal = concatMap takeSteps
    where takeSteps done@(True, _, _, _) = [done]
          takeSteps (False, costSoFar, seen, last) = map takeStep $ nextMoves grid last
            where takeStep move@(_,pos,_) = (pos==goal, costSoFar + cost last move, move:seen, move)

done :: Path -> Bool
done (d,_,_,_) = d

allRoutes :: Grid -> [Path]
allRoutes grid = snd $ solve (M.empty,[(False, 0, [start], start)])
    where best = partOne grid
          start = (Step, head $ M.keys $ M.filter ('S' ==) grid, (1,0))
          end = head $ M.keys $ M.filter ('E' ==) grid
          solve (seen,paths) = if all done paths then (seen,paths) else iterate (seen,paths)
          iterate (seen,paths) = let f = filtered seen paths in solve (record seen f,f) -- solve $ traceShow (length f) (record seen f,f)
          record = foldl (\m (_,c,_,l) -> M.insert l c m)
          filtered seen paths = filter (\p -> done p || (worth p && not (redundant seen p))) $ advance grid end paths
          redundant seen (_,c,_,last) = M.findWithDefault (best*2) last seen <= c
          worth (done,cost,_,_) = cost <= best

getSteps :: Path -> [Step]
getSteps (_,_,steps,_) = steps

partTwo :: Grid -> Int
partTwo grid = length $ nub $ map getPos $ concatMap getSteps $ allRoutes grid

main = do
    maze <- M.filter ('.' /=) . asMap . lines <$> readFile "day16.txt"
    print $ partOne maze
    print $ partTwo maze
