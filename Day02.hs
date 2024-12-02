module Day02 where

import Data.List (sort)

safe :: [Int] -> Bool
safe report = sorted && changing && smallSteps
    where sorted = sort report == report || sort report == reverse report
          differences = map abs $ zipWith (-) report (tail report)
          changing = all (>0) differences
          smallSteps = all (<= 3) differences

partOne :: [[Int]] -> Int
partOne = length . filter safe

main = do
    reports <- map ((map read) . words) . lines <$> readFile "day02.txt"
    print $ partOne reports