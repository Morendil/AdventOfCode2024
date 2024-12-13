module Day13 where

import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (sort)

type Pos = (Integer,Integer)
type Machine = ((Pos,Pos),Pos)

number :: ReadP Integer
number = read <$> many1 (satisfy isNumber)

button :: ReadP Pos
button = do
    string "Button "
    get
    string ": X+"
    x <- number
    string ", Y+"
    y <- number
    return (x,y)

prize :: ReadP Pos
prize = do
    string "Prize: X="
    x <- number
    string ", Y="
    y <- number
    return (x,y)

machine :: ReadP Machine
machine = do
    a <- button
    string "\n"
    b <- button
    string "\n"
    p <- prize
    return ((a,b),p)

arcade :: ReadP [Machine]
arcade = sepBy1 machine (string "\n\n")

solve :: Machine -> Maybe Pos
solve (((ax,ay),(bx,by)),(px,py)) = if snd an == 0 && snd bn == 0 then Just (fst an, fst bn) else Nothing
    where an = (px*by - py*bx) `divMod` (ax*by-ay*bx)
          bn = (px*ay - py*ax) `divMod` (bx*ay-by*ax)

cost :: Pos -> Integer
cost (a,b) = 3*a + b

partOne :: [Machine] -> Integer
partOne = sum . map cost . mapMaybe solve

partTwo :: [Machine] -> Integer
partTwo = partOne . map respec

respec :: Machine -> Machine
respec (claw,(px,py)) = (claw,(px+10000000000000,py+10000000000000))

main = do
    machines <- fromJust . parseMaybe arcade <$> readFile "day13.txt"
    print $ partOne machines
    print $ partTwo machines

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
