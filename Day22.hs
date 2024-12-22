module Day22 where

import Data.Bits
import Data.List (tails)
import qualified Data.Map as M

type SeqMap = M.Map [Int] Int

evolve :: Int -> Int
evolve = stepThree . stepTwo . stepOne
    where stepOne secret = prune $ mix (secret * 64) secret
          stepTwo secret = prune $ mix (secret `div` 32) secret
          stepThree secret = prune $ mix (secret * 2048) secret
          mix = xor
          prune = (`mod` 16777216)

partOne :: [Int] -> Int
partOne = sum . map (last . take 2001 . iterate evolve)

partTwo :: [Int] -> Int
partTwo = maximum . M.unionsWith (+) . map priceMap

prices :: Int -> [([Int],Int)]
prices secret = zip seqs (drop 4 spots)
    where spots = take 2001 $ map (`mod` 10) $ iterate evolve secret
          diffs = zipWith (-) (tail spots) spots
          seqs = map (take 4) (tails diffs)

priceMap :: Int -> SeqMap
priceMap = M.fromListWith (\a b -> b) . prices

main = do
    secrets <- map read . lines <$> readFile "day22.txt"
    print $ partOne secrets
    print $ partTwo secrets