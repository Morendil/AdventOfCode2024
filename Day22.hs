module Day22 where

import Data.Bits

evolve :: Int -> Int
evolve = stepThree . stepTwo . stepOne
    where stepOne secret = prune $ mix (secret * 64) secret
          stepTwo secret = prune $ mix (secret `div` 32) secret
          stepThree secret = prune $ mix (secret * 2048) secret
          mix = xor
          prune = (`mod` 16777216)

partOne :: [Int] -> Int
partOne = sum . map (last . take 2001 . iterate evolve)

main = do
    secrets <- map read . lines <$> readFile "day22.txt"
    print $ partOne secrets