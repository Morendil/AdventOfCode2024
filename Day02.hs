module Day02 where

import Data.List (sort)
import Data.Foldable (toList)
import Data.Sequence (fromList, deleteAt)

safe :: [Int] -> Bool
safe report = sorted && changing && smallSteps
    where sorted = sort report == report || sort report == reverse report
          differences = map abs $ zipWith (-) report (tail report)
          changing = all (>0) differences
          smallSteps = all (<= 3) differences

almostSafe :: [Int] -> Bool
almostSafe = any safe . allDeletions
    where allDeletions report = map (toList . flip deleteAt (fromList report)) [0..length report]

partOne :: [[Int]] -> Int
partOne = length . filter safe

partTwo :: [[Int]] -> Int
partTwo = length . filter almostSafe

main = do
    reports <- map ((map read) . words) . lines <$> readFile "day02.txt"
    print $ partOne reports
    print $ partTwo reports