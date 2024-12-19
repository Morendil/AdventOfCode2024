module Day19 where

import Algorithm.Search
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Char (isAlpha)

type Towels = ([String],[String])

parse :: [String] -> Towels
parse all = (designs $ head all, drop 2 all)
    where designs = map (filter isAlpha) . words

solve :: [String] -> String -> Maybe [String]
solve stripes towel = bfs succ (towel ==) ""
    where succ state = filter (`isPrefixOf` towel) $ map (state ++) stripes

partOne :: Towels -> Int
partOne (patterns, designs) = length $ mapMaybe (solve patterns) designs

main = do
    towels <- parse . lines <$> readFile "day19.txt"
    print $ partOne towels