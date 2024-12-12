module Day12 where

import Data.Graph
import Data.Tree
import qualified Data.Map as M
import Data.List (nub)
import Data.Tuple (swap)
type Pos = (Int, Int)
type Grid = M.Map Pos Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x,y),(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

otherNeighbors :: (Int,Int) -> [(Int,Int)]
otherNeighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

findEdges :: Grid -> [Edge]
findEdges grid = concatMap edgesNs (M.assocs grid)
    where maxX = maximum $ map fst $ M.keys grid
          asVertex (x,y) = y*(maxX+1)+x
          edgesNs (p,v) = map ((asVertex p,) . asVertex) (similarNs (p,v))
          similarNs (p,v) = filter (\n -> M.findWithDefault '.' n grid == self) (neighbors p)
            where self = M.findWithDefault '.' p grid

perimeter :: Grid -> [Pos] -> Int
perimeter grid ps = length different
    where allNs = concatMap otherNeighbors ps
          self = M.findWithDefault '.' (head ps) grid
          different = filter (\p -> M.findWithDefault '.' p grid /= self) allNs

partOne :: Grid -> Int
partOne gardens = sum $ zipWith (*) (map length regions) (map (perimeter gardens) regions)
    where graph = buildG (0, length gardens-1) $ findEdges gardens
          maxX = maximum $ map snd $ M.keys gardens
          fromVertex v = swap $ v `divMod` (maxX+1)
          regions =  map (map fromVertex . flatten) $ dff graph

main = do
    gardens <- asMap . lines <$> readFile "day12.txt"
    print $ partOne gardens
