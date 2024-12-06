module Day06 where
import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (fromJust)

type Pos = (Int,Int)
type Dir = (Int,Int)

pathFrom :: (Int,Int) -> [(Int,Int)] -> (Pos, Dir) -> [(Pos,Dir)]
pathFrom maxes grid start = reverse $ extendPath grid start []
    where extendPath grid start previous = case step maxes grid start of
            Nothing -> previous
            Just newPos -> extendPath grid newPos (newPos:previous)

step :: (Int,Int) -> [(Int,Int)] -> (Pos, Dir) -> Maybe (Pos, Dir)
step (maxx,maxy) obstacles (pos@(px,py), dir@(dx,dy))
  | out = Nothing
  | bump = Just (pos, turn dir)
  | otherwise = Just ((nx,ny),dir)
  where
      (nx, ny) = (px + dx, py + dy)
      out = nx < 0 || nx >= maxx || ny < 0 || ny >= maxy
      bump = (nx, ny) `elem` obstacles

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
          maxes = (length (head grid) - 1, length grid)

obstacles :: [String] -> [(Int,Int)]
obstacles grid = [(x,y) | x <- [0..length (head grid)-1], y <- [0..length grid-1], grid !! y !! x == '#']

main = do
    grid <- lines <$> readFile "day06.txt"
    print $ partOne grid