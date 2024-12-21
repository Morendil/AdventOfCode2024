module Day21 where

import Data.Tuple (swap)
import Data.Maybe (fromJust)

import Control.Monad ( replicateM, zipWithM )
import qualified Data.Map as M
import Data.List (transpose)
import Data.Char (isDigit)

type Pos = (Int,Int)
type Grid = M.Map Pos Char
type Where = M.Map Char Pos
type Path = (Pos,[(Char,Pos)])

type Paths = M.Map (Char,Char) [String]
type Costs = M.Map ((Char,Char),Int) Int

numberPad :: Grid
numberPad = asMap ["789","456","123",".0A"]

numberWhere :: Where
numberWhere = M.fromList $ map swap $ M.assocs numberPad

dirPad :: Grid
dirPad = asMap [".^A","<v>"]

dirWhere :: Where
dirWhere = M.fromList $ map swap $ M.assocs dirPad

dirs :: [(Char,Pos)]
dirs = [('>',(1,0)),('<',(-1,0)),('^',(0,-1)),('v',(0,1))]

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)

add :: Pos -> Pos -> Pos
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

step :: Grid -> Pos -> Path -> [Path]
step g (x2,y2) ((x1,y1),soFar) = map (\s@(c,o) -> (add o (x1,y1),s:soFar)) $ filter valid dirs
    where valid (c,(ox,oy)) = ((signum (x2-x1) == ox && oy == 0) || (signum (y2-y1) == oy && ox == 0)) && notGap (ox,oy)
          notGap (ox,oy) = M.findWithDefault '.' (x1+ox,y1+oy) g /= '.'

advance :: Grid -> Pos -> [Path] -> [Path]
advance g goal = concatMap (step g goal)

paths :: Grid -> Where -> Char -> Char -> [String]
paths g w d1 d2 = map presses paths
    where presses = reverse . ('A' :) . map fst . snd
          paths = last $ take (d+1) $ iterate (advance g p2) [(p1,[])]
          p1 = fromJust $ M.lookup d1 w
          p2 = fromJust $ M.lookup d2 w
          d = dist p1 p2

numberPaths :: Char -> Char -> [String]
numberPaths = paths numberPad numberWhere

dirPaths :: Char -> Char -> [String]
dirPaths = paths dirPad dirWhere

dirPathsMemo = M.fromList [((d1,d2),dirPaths d1 d2) | (d1,d2) <- cross "A<>^v"]

cross :: Eq a => [a] -> [(a, a)]
cross n = [(x,y) | x <-n, y <- n]

layerOne :: String -> [String]
layerOne code = map concat $ zipWithM numberPaths ('A':code) code

complexity :: Int -> String -> Int
complexity level code = minimum $ map (cost level) $ layerOne code

numericPart :: String -> Int
numericPart code = read $ filter isDigit code

value :: Int -> String -> Int
value level code = complexity level code * numericPart code

partOne :: [String] -> Int
partOne = sum . map (value 1)

partTwo :: [String] -> Int
partTwo = sum . map (value 24)

cost :: Int -> String -> Int
cost 0 code = length code
cost n code = sum $ zipWith (\d1 d2 -> fromJust $ M.lookup ((d1,d2),n) layer) ('A':code) code
    where layer = buildLayer n

dirPairs = cross "A<>^v"

buildLayer :: Int -> Costs
buildLayer 0 = M.fromList [(((d1,d2),0),minimum $ map length $ fromJust $ M.lookup (d1,d2) dirPathsMemo) | (d1,d2) <- dirPairs]
buildLayer n = M.fromList $ top ++ M.assocs prev
    where prev = buildLayer (n-1)
          top = [((p,n), minimum $ map costOf (pathsOf p)) | p <- dirPairs]
          pathsOf p = fromJust $ M.lookup p dirPathsMemo
          costOf :: String -> Int
          costOf path = sum $ zipWith (\d1 d2 -> fromJust $ M.lookup ((d1,d2),n-1) prev) ('A':path) path

main = do
    codes <- lines <$> readFile "day21.txt"
    print $ partOne codes
    print $ partTwo codes