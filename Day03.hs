module Day03 where

import Text.Regex.TDFA

value :: String -> Int
value mul = product $ map read $ getAllTextMatches (mul =~"[0-9]{1,3}")

partOne :: String -> Int
partOne program = sum $ map value muls
    where muls :: [String]
          muls = getAllTextMatches (program =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)")

partTwo :: String -> Int
partTwo program = snd $ foldl addValue (True,0) muls
    where muls :: [String]
          muls = getAllTextMatches (program =~ "(mul\\(([0-9]{1,3}),([0-9]{1,3})\\))|do\\(\\)|don't\\(\\)")
          addValue (enabled, total) "don't()" = (False,total)
          addValue (enabled, total) "do()" = (True,total)
          addValue (True, total) mul = (True,total+value mul)
          addValue (False, total) mul = (False, total)

main = do
    program <- readFile "day03.txt"
    print $ partOne program
    print $ partTwo program