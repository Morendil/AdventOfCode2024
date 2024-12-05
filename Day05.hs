module Day05 where

import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (elemIndex)

ordering :: ReadP (Int, Int)
ordering = (,) <$> number <*> (string "|" *> number)

rules :: ReadP [(Int,Int)]
rules = sepBy1 ordering (string "\n")

updates :: ReadP [[Int]]
updates = sepBy1 (sepBy1 number (string ",")) (string "\n")

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

spec :: ReadP ([(Int,Int)], [[Int]])
spec = (,) <$> rules <*> (string "\n\n" *> updates)

partOne :: ([(Int,Int)], [[Int]]) -> Int
partOne (orderings,pageRuns) = sum $ map middle $ filter (obeys orderings) pageRuns
    where middle pageRun = pageRun !! (length pageRun `div` 2)
          obeys orderings run = all (sameOrder run) orderings
          sameOrder run (before, after) = fromMaybe True $ (<) <$> elemIndex before run <*> elemIndex after run

main = do
    spec <- fromJust . parseMaybe spec <$> readFile "day05.txt"
    print $ partOne spec

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
