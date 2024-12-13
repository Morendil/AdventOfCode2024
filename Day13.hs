module Day13 where

import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust)
import Data.List (sort)

type Pos = (Int,Int)
type Machine = ((Pos,Pos),Pos)

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

button :: ReadP (Int, Int)
button = do
    string "Button "
    get
    string ": X+"
    x <- number
    string ", Y+"
    y <- number
    return (x,y)

prize :: ReadP (Int, Int)
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

winners :: Machine -> [(Int,Int)]
winners machine = filter winning [(an,bn) | an <- [0..100], bn <- [0..100]]
    where (((ax,ay),(bx,by)),(px,py)) = machine
          winning (an,bn) = (an*ax+bn*bx,an*ay+bn*by) == (px,py)

cost :: Pos -> Int
cost (a,b) = 3*a + b

partOne :: [Machine] -> Int
partOne = sum . concatMap (take 1 . sort . map cost . winners)

main = do
    machines <- fromJust . parseMaybe arcade <$> readFile "day13.txt"
    print $ partOne machines

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
