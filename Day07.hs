import Data.Char (isNumber)
import Control.Monad (replicateM)

type Equation = [Int]
type Operation = Int -> Int -> Int
type Application = ([Operation],[Int])

apply :: Application -> Int
apply ([],x:xs) = x
apply (op:ops,x:y:xs) = apply (ops, op x y:xs)

equates :: Equation -> [Operation] -> Bool
equates eq ops = head eq == apply (ops,tail eq)

canSolve :: Equation -> Bool
canSolve eq = any (equates eq) candidates
    where candidates = replicateM (length eq - 2) [(*),(+)]

partOne :: [Equation] -> Int
partOne = sum . map head . filter canSolve

main = do
    equations :: [[Int]] <- map (map (read . filter isNumber) . words) . lines <$> readFile "day07.txt"
    print $ partOne equations