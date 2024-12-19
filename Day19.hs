module Day19 where

import Algorithm.Search
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Char (isAlpha)

import qualified Data.Map as M
import Data.List.HT (takeUntil)

type Towels = ([String],[String])
type Ways = M.Map String Int

parse :: [String] -> Towels
parse all = (designs $ head all, drop 2 all)
    where designs = map (filter isAlpha) . words

solve :: [String] -> String -> Maybe [String]
solve stripes towel = bfs succ (towel ==) ""
    where succ state = filter (`isPrefixOf` towel) $ map (state ++) stripes

partOne :: Towels -> Int
partOne (patterns, designs) = length $ mapMaybe (solve patterns) designs

complete :: [String] -> String -> Ways -> Ways
complete stripes towel ways = M.fromListWith (+) $ alreadyThere ++ concatMap next assocs
    where next (prefix, count) = map (,count) $ filter (`isPrefixOf` towel) $ map (prefix ++) stripes
          alreadyThere = filter (\(prefix, count) -> prefix == towel) assocs
          assocs = M.assocs ways

howMany :: [String] -> String -> Int
howMany stripes towel = sum $ M.elems $ last $ takeUntil (\ways -> null ways || M.keys ways == [towel]) $ steps
    where steps = iterate (complete stripes towel) $ M.fromList [("",1)]

partTwo :: Towels -> Int
partTwo (patterns, designs) = sum $ map (howMany patterns) designs

main = do
    towels <- parse . lines <$> readFile "day19.txt"
    print $ partOne towels
    print $ partTwo towels