module Day04 where
import Data.List (intersect)

wordOffsets (x,y) = [
        [(x,y+o) | o <- [0..3]], -- s
        [(x,y-o) | o <- [0..3]], -- n
        [(x+o,y) | o <- [0..3]], -- e
        [(x-o,y) | o <- [0..3]], -- w
        [(x+o,y+o) | o <- [0..3]], -- se
        [(x-o,y+o) | o <- [0..3]], -- sw
        [(x+o,y-o) | o <- [0..3]], -- ne
        [(x-o,y-o) | o <- [0..3]] -- nw
    ]

xOffsets (x,y) = [
        [(x+o,y+o) | o <- [-1,0,1]], -- se
        [(x-o,y-o) | o <- [-1,0,1]], -- nw
        [(x-o,y+o) | o <- [-1,0,1]], -- sw
        [(x+o,y-o) | o <- [-1,0,1]] -- ne
    ]

at :: [String] -> (Int, Int) -> Char
at grid (x,y) | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = '.'
at grid (x,y) = grid !! y !! x

locs :: [String] -> [(Int, Int)]
locs grid = [(x,y) | x <- [0..length (head grid) -1], y <- [0..length grid-1]]

partOne :: [String] -> Int
partOne grid = length $ filter ("XMAS" ==) $ concat $ makeWordsEverywhere grid
    where makeWords grid (x,y) = map (at grid)
          makeAllWords grid (x,y) = map (makeWords grid (x,y)) (wordOffsets (x,y))
          makeWordsEverywhere grid = map (makeAllWords grid) (locs grid)

partTwo :: [String] -> Int
partTwo grid = length $ intersect se sw
    where makeWords grid (x,y) = map (at grid)
          findMas (x,y) = elem "MAS" . map (makeWords grid (x,y))
          hasMasSE (x,y) = findMas (x,y) (take 2 $ xOffsets (x,y))
          hasMasSW (x,y) = findMas (x,y) (drop 2 $ xOffsets (x,y))
          se = filter hasMasSE (locs grid)
          sw = filter hasMasSW (locs grid)

main = do
    grid <- lines <$> readFile "day04.txt"
    print $ partOne grid
    print $ partTwo grid