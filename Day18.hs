module Day18 where

import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import Algorithm.Search (aStar)

type Pos = (Int,Int)

pair :: ReadP (Int,Int)
pair = (,) <$> number <*> (string "," *> number)

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

partOne :: [Pos] -> Int
partOne bytes = fst $ fromJust $ aStar (filter valid . neighbors) (const $ const 1) dist ((maxX,maxY) ==) (0,0)
    where maxX = maximum $ map fst bytes
          maxY = maximum $ map snd bytes
          valid (x,y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY && (x,y) `notElem` bytes
          dist (x,y) = (maxX-x)+(maxY-y)

main = do
    bytes <- fromJust . parseMaybe (sepBy1 pair (string "\n")) <$> readFile "day18.txt"
    print $ partOne $ take 1024 bytes

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
