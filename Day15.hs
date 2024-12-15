module Day15 where

import qualified Data.Map as M
import Data.List.HT (takeUntil)

type Pos = (Int,Int)
type Grid = M.Map Pos Char
type Warehouse = (Grid, String)

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

parse :: String -> Warehouse
parse spec = (grid, cmd)
    where everything = lines spec
          grid = M.filter ('.' /=) $ asMap gridLines
          gridLines = takeWhile ((==) "#" . take 1) everything
          cmd = concat $ drop (length gridLines+1) everything

add :: Pos -> Pos -> Pos
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

offset :: Char -> Pos
offset '^' = (0,-1)
offset '<' = (-1,0)
offset '>' = (1,0)
offset 'v' = (0,1)

moveBot :: Grid -> Pos -> Grid
moveBot grid dir = if canMove then grid' else grid
    where heading = iterate (add dir) bot
          at pos = M.findWithDefault '.' pos grid
          spots = takeUntil ((`elem` "#.") . at) heading
          canMove = at (last spots) == '.'
          grid' = M.mapKeys (\p -> if p `elem` spots then add p dir else p) grid
          bot = head $ M.keys $ M.filter ('@' ==) grid

display :: Grid -> [String]
display grid = [ [ M.findWithDefault '.' (x,y) grid | x <- [0..xMax]] | y <- [0..yMax]]
  where (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)

execute :: Grid -> String -> Grid
execute grid = foldl moveBot grid . map offset

gps :: Grid -> Int
gps grid = sum $ map (\(x,y) -> 100*y+x) $ M.keys $ M.filter ('O' ==) grid

partOne :: Grid -> String -> Int
partOne grid cmd = gps $ execute grid cmd

main = do
    (grid, cmd) <- parse <$> readFile "day15.txt"
    print $ partOne grid cmd
