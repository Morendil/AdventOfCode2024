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

middle :: [a] -> a
middle pageRun = pageRun !! (length pageRun `div` 2)

sameOrder :: Eq a => [a] -> (a, a) -> Bool
sameOrder run (before, after) = fromMaybe True $ (<) <$> elemIndex before run <*> elemIndex after run

obeys :: Eq a => [(a, a)] -> [a] -> Bool
obeys orderings run = all (sameOrder run) orderings

partOne :: ([(Int,Int)], [[Int]]) -> Int
partOne (orderings,pageRuns) = sum $ map middle $ filter (obeys orderings) pageRuns

swap :: [a] -> (Int, Int) -> [a]
swap xs (f,s) = zipWith (\x y -> 
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs

partTwo :: ([(Int,Int)], [[Int]]) -> Int
partTwo (orderings,pageRuns) = sum $ map (middle . converge (reorder orderings)) incorrects
    where reorder orderings run = foldl swapIfNeeded run orderings
          swapIfNeeded run order = if sameOrder run order then run else doSwap run order
          doSwap run order = swap run (fromJust $ elemIndex (fst order) run, fromJust $ elemIndex (snd order) run)
          incorrects = filter (not . obeys orderings) pageRuns

main = do
    spec <- fromJust . parseMaybe spec <$> readFile "day05.txt"
    print $ partOne spec
    print $ partTwo spec

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
