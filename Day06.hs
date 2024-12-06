module Day06 where
import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (fromJust)

type Pos = (Int,Int)
type Dir = (Int,Int)

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f x = case f x of Nothing -> []
                             Just y  -> y : repeatedly f y

step :: [String] -> (Pos, Dir) -> Maybe (Pos, Dir)
step grid (pos@(px,py), dir@(dx,dy))
  | out = Nothing
  | bump = Just (pos, turn dir)
  | otherwise = Just ((nx,ny),dir)
  where
      (nx, ny) = (px + dx, py + dy)
      out = nx < 0 || nx >= length (head grid) || ny < 0 || ny >= length grid
      bump = grid !! ny !! nx == '#'
      turn (0,-1) = (1,0)
      turn (1,0) = (0,1)
      turn (0,1) = (-1,0)
      turn (-1,0) = (0,-1)

partOne :: [[Char]] -> Int
partOne grid = length $ nub $ map fst (start : repeatedly (step grid) start)
    where start = (guard, (0,-1))
          guard = (gx,gy)
          gy = fromJust $ findIndex (elem '^') grid
          gx = fromJust $ elemIndex '^' (grid !! gy)

main = do
    grid <- lines <$> readFile "day06.txt"
    print $ partOne grid