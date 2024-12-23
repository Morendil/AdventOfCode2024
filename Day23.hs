module Day23 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, tails)
import Data.Tuple (swap)

asPair :: String -> (String, String)
asPair s = (take 2 s, drop 3 s)

partOne :: [(String,String)] -> Int
partOne links = length $ nub $ concatMap three ts
    where pairs n = [(x,y) | (x:ys) <- tails n, y <- ys]
          nodes = nub (map fst links ++ map snd links)
          undir = links ++ map swap links
          ts = filter ((=='t').head) nodes
          m = M.fromListWith (++) $ map (\(a,b)->(a,[b])) undir
          three t = map (setWith t) $ filter (`elem` undir) $ pairs $ M.findWithDefault [] t m
          setWith a (b,c) = S.fromList [a,b,c]

main = do
    links <- map asPair . lines <$> readFile "day23.txt"
    print $ partOne links