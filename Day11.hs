module Day11 where

import Data.List (nub)
import qualified Data.Map as M

step :: M.Map Int Int -> M.Map Int Int
step stones = M.fromListWith (+) $ concatMap stepPair $ M.assocs stones
    where stepPair (num,total) = map (,total) $ stepStone num
          stepStone 0 = [1]
          stepStone stone | evenDs = [read (take (ds `div` 2) num), read (drop (ds `div` 2) num)]
            where ds = length num
                  evenDs = even ds
                  num = show stone
          stepStone stone = [2024*stone]

partOne :: [Int] -> Int
partOne = solve 25

partTwo :: [Int] -> Int
partTwo = solve 75

solve :: Int -> [Int] -> Int
solve n = sum . last . take (n+1) . iterate step . M.fromList . map (, 1)

main = do
    stones <- map read . words <$> readFile "day11.txt"
    print $ partOne stones
    print $ partTwo stones    