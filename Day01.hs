module Day01 where

import Data.List (sort, transpose, findIndices)

partOne :: ([Int], [Int]) -> Int
partOne (l1, l2) = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

partTwo :: ([Int], [Int]) -> Int
partTwo (l1, l2) = sum $ zipWith (*) (map (occurrences l2) l1) l1
    where occurrences l v = length $ findIndices (v==) l

main = do
    let toPair (x:y:rest) = (x,y)
        toPair _ = error "Nope"
    lists <- toPair . transpose . map ((map read) . words) . lines <$> readFile "day01.txt"
    print $ partOne lists
    print $ partTwo lists
