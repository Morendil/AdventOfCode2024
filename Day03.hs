module Day03 where

import Text.Regex.TDFA

partOne :: String -> Int
partOne program = sum $ map value muls
    where muls :: [String]
          muls = getAllTextMatches (program =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")
          value mul = product $ map read $ getAllTextMatches (mul =~"[0-9]{1,3}")

main = do
    program <- readFile "day03.txt"
    print $ partOne program