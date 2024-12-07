module Day06 where
import Data.List (elemIndex, findIndex, nub, (\\))
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Map as M

type Path = (Bool, [(Pos,Dir)])
type Crumbs = M.Map (Pos,Dir) Path

type Pos = (Int,Int)
type Dir = (Int,Int)

determinePath :: (Int,Int) -> [(Int,Int)] -> (Pos, Dir) -> Path
determinePath maxes bumps start = extendPath bumps start []
    where extendPath bumps start previous = case step maxes bumps start of
            Nothing -> (False, reverse previous)
            Just newPos -> if newPos `elem` previous then (True, reverse previous)
                           else extendPath bumps newPos (newPos:previous)

pathFrom :: (Int,Int) -> [(Int,Int)] -> (Pos, Dir) -> [(Pos,Dir)]
pathFrom maxes bumps start = reverse $ extendPath bumps start []
    where extendPath bumps start previous = case step maxes bumps start of
            Nothing -> previous
            Just newPos -> if newPos `elem` previous then error "Hey, a loop !" else extendPath bumps newPos (newPos:previous)

step :: (Int,Int) -> [(Int,Int)] -> (Pos, Dir) -> Maybe (Pos, Dir)
step (maxx,maxy) bumps (pos@(px,py), dir@(dx,dy))
  | out = Nothing
  | bump = Just (pos, turn dir)
  | otherwise = Just ((nx,ny),dir)
  where
      (nx, ny) = (px + dx, py + dy)
      out = nx < 0 || nx > maxx || ny < 0 || ny > maxy
      bump = (nx, ny) `elem` bumps

turn :: (Int, Int) -> (Int, Int)
turn (0,-1) = (1,0)
turn (1,0) = (0,1)
turn (0,1) = (-1,0)
turn (-1,0) = (0,-1)

partOne :: [[Char]] -> Int
partOne grid = length $ nub $ map fst (start : pathFrom maxes (obstacles grid) start)
    where start = (guard, (0,-1))
          guard = (gx,gy)
          gy = fromJust $ findIndex (elem '^') grid
          gx = fromJust $ elemIndex '^' (grid !! gy)
          maxes = (length (head grid) - 1, length grid - 1)

obstacles :: [String] -> [(Int,Int)]
obstacles grid = [(x,y) | x <- [0..length (head grid)-1], y <- [0..length grid-1], grid !! y !! x == '#']

partTwo :: [[Char]] -> Int
partTwo grid = length $ mapMaybe (\loc -> if fst $ determinePath maxes (loc:bumps) start then Just loc else Nothing) locations
    where path = pathFrom maxes bumps start
          locations = nub $ map fst path
          bumps = obstacles grid
          start = (guard, (0,-1))
          guard = (gx,gy)
          gy = fromJust $ findIndex (elem '^') grid
          gx = fromJust $ elemIndex '^' (grid !! gy)
          maxes = (length (head grid) - 1, length grid)

main = do
    grid <- lines <$> readFile "day06.txt"
    print $ partOne grid
    print $ partTwo grid