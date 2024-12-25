module Day25 where

import Data.Text (pack, unpack, splitOn)
import Data.List (transpose, partition)
import Data.Bifunctor (bimap)

main = do
    locksAndKeys <- map (lines . unpack) . splitOn (pack "\n\n") . pack <$> readFile "day25.txt"
    let vert = partition (all (=='#').head) locksAndKeys
        horz = bimap (map transpose) (map (map reverse . transpose)) vert
        (lockPins,keyPins) = bimap measure measure horz
        measure = map $ map (pred . length . takeWhile (=='#'))
    print $ length $ filter (all (<6)) $ [zipWith (+) lock key | lock <- lockPins, key <- keyPins]