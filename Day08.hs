module Day08 where

import qualified Data.Map as M
import Data.List

type Grid = M.Map (Int, Int) Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

partOne :: Grid -> Int
partOne grid = length $ filter (`M.member` grid) antinodes
    where frequencies = nub $ M.elems $ M.filter (/= '.') grid
          nodes f = M.keys $ M.filter (== f) grid
          pairs n = [(x,y) | (x:ys) <- tails n, y <- ys]
          antis ((n1x,n1y),(n2x,n2y)) = [(2*n1x-n2x,2*n1y-n2y),(2*n2x-n1x,2*n2y-n1y)]
          antinodes = nub $ concatMap (concatMap antis . pairs . nodes) frequencies

partTwo :: Grid -> Int
partTwo grid = length $ filter inLineWithAnyPair $ M.keys grid
    where frequencies = nub $ M.elems $ M.filter (/= '.') grid
          nodes f = M.keys $ M.filter (== f) grid
          pairs n = [(x,y) | (x:ys) <- tails n, y <- ys]
          allPairs = concatMap (pairs . nodes) frequencies
          inLineWithAnyPair xy = any (inLine xy) allPairs
          inLine (x,y) ((n1x,n1y),(n2x,n2y)) = forY == forX && snd forY == 0 && snd forX == 0
            where forY = (y - n1y) `divMod` (n2y-n1y)
                  forX = (x - n1x) `divMod` (n2x-n1x)

main = do
    grid <- asMap . lines <$> readFile "day08.txt"
    print $ partOne grid
    print $ partTwo grid