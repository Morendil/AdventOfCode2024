module Day06 where
import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (fromJust)

type Pos = (Int,Int)
type Dir = (Int,Int)

pathFrom :: [String] -> (Pos, Dir) -> [(Pos,Dir)]
pathFrom grid start = reverse $ extendPath grid start []
    where extendPath grid start previous = case step grid start of
            Nothing -> previous
            Just newPos -> extendPath grid newPos (newPos:previous)

step :: [String] -> (Pos, Dir) -> Maybe (Pos, Dir)
step grid (pos@(px,py), dir@(dx,dy))
  | out = Nothing
  | bump = Just (pos, turn dir)
  | otherwise = Just ((nx,ny),dir)
  where
      (nx, ny) = (px + dx, py + dy)
      out = nx < 0 || nx >= length (head grid) || ny < 0 || ny >= length grid
      bump = grid !! ny !! nx == '#'

turn :: (Int, Int) -> (Int, Int)
turn (0,-1) = (1,0)
turn (1,0) = (0,1)
turn (0,1) = (-1,0)
turn (-1,0) = (0,-1)

partOne :: [[Char]] -> Int
partOne grid = length $ nub $ map fst (start : pathFrom grid start)
    where start = (guard, (0,-1))
          guard = (gx,gy)
          gy = fromJust $ findIndex (elem '^') grid
          gx = fromJust $ elemIndex '^' (grid !! gy)

main = do
    grid <- lines <$> readFile "day06.txt"
    print $ partOne grid