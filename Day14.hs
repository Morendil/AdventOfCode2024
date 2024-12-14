module Day14 where

import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Text (isInfixOf, pack)

type Pos = (Int,Int)
type Vel = (Int,Int)

number :: ReadP Int
number = do
  sign <- option ' ' (char '-')
  magnitude <- many1 $ satisfy isNumber
  return $ read (sign : magnitude)

pair :: ReadP (Int,Int)
pair = (,) <$> number <*> (string "," *> number)

robot :: ReadP (Pos,Vel)
robot = do
    string "p="
    pos <- pair
    string " v="
    vel <- pair
    return (pos,vel)

simulate :: Int -> (Int,Int) -> (Pos,Vel) -> Pos
simulate n (w,h) ((px,py),(vx,vy)) = ((px+n*vx)`mod`w,(py+n*vy)`mod`h)

partOne :: (Int,Int) -> [(Pos,Vel)] -> Int
partOne (w,h) bots = product $ map length [q1,q2,q3,q4]
    where final = map (simulate 100 (w,h)) bots
          q1 = filter (\(px,py) -> px < w `div` 2 && py < h `div` 2) final
          q2 = filter (\(px,py) -> px > w `div` 2 && py < h `div` 2) final
          q3 = filter (\(px,py) -> px < w `div` 2 && py > h `div` 2) final
          q4 = filter (\(px,py) -> px > w `div` 2 && py > h `div` 2) final

display :: (Int,Int) -> [Pos] -> [String]
display (w,h) bots = [[if (x,y) `elem` bots then '*' else '.' | x <- [0..w-1]] | y <- [0..h-1]]

partTwo :: [(Pos,Vel)] -> Int
partTwo bots = fromJust $ findIndex (any ((pack "**********" `isInfixOf` ) . pack) . display (101,103)) frames
    where frame n = map (simulate n (101,103)) bots
          frames = map frame [0..101*103]

main = do
    robots <- fromJust . parseMaybe (sepBy1 robot (string "\n")) <$> readFile "day14.txt"
    print $ partOne (101,103) robots
    print $ partTwo robots

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
