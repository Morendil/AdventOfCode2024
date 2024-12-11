module Day11 where

step :: [Int] -> [Int]
step = concatMap stepStone
    where stepStone 0 = [1]
          stepStone stone | evenDs = [read (take (ds `div` 2) num), read (drop (ds `div` 2) num)]
            where ds = length num
                  evenDs = even ds
                  num = show stone
          stepStone stone = [2024*stone]

partOne :: [Int] -> Int
partOne stones = length $ last $ take 26 $ iterate step stones

main = do
    stones <- map read . words <$> readFile "day11.txt"
    print $ partOne stones