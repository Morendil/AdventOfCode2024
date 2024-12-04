module Day04 where

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

at :: [String] -> (Int, Int) -> Char
at grid (x,y) | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = '.'
at grid (x,y) = grid !! y !! x

partOne :: [String] -> Int
partOne grid = length $ filter ("XMAS" ==) $ concat $ makeWordsEverywhere grid
    where locs grid = [(x,y) | x <- [0..length (head grid) -1], y <- [0..length grid-1]]
          makeWords grid (x,y) = map (at grid)
          makeAllWords grid (x,y) = map (makeWords grid (x,y)) (wordOffsets (x,y))
          makeWordsEverywhere grid = map (makeAllWords grid) (locs grid)

main = do
    grid <- lines <$> readFile "day04.txt"
    print $ partOne grid