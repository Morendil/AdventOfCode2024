module Day18 where

import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust, isNothing)
import Data.Char (isNumber)
import Algorithm.Search (aStar)

type Pos = (Int,Int)

pair :: ReadP (Int,Int)
pair = (,) <$> number <*> (string "," *> number)

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

solve :: Int -> [Pos] -> Maybe (Int, [Pos])
solve n bytes = aStar (filter valid . neighbors) (const $ const 1) dist ((maxX,maxY) ==) (0,0)
    where maxX = maximum $ map fst bytes
          maxY = maximum $ map snd bytes
          takenBytes = take n bytes
          valid (x,y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY && (x,y) `notElem` takenBytes
          dist (x,y) = (maxX-x)+(maxY-y)

partOne :: Int -> [Pos] -> Int
partOne n bytes = fst $ fromJust $ solve n bytes

partTwo :: [Pos] -> (Int,Int)
partTwo bytes = bytes !! bisect bytes 0 (length bytes - 1)
    where bisect range lo hi
            | hi-lo <= 1 = hi-1
            | isNothing $ solve (lo+((hi-lo)`div`2)) bytes = bisect range lo (lo+((hi-lo)`div`2)-1)
            | otherwise = bisect range (lo+((hi-lo)`div`2)+1) hi

main = do
    bytes <- fromJust . parseMaybe (sepBy1 pair (string "\n")) <$> readFile "day18.txt"
    print $ partOne 1024 bytes
    print $ partTwo bytes

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
