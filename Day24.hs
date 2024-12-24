module Day24 where

import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (fromJust)
import Data.List (nub, sort, unfoldr)
import Data.Bits

type Initial = (String, Int)
type Gate = (String, (String, String))
type Circuit = M.Map String Gate
type Values = M.Map String Int

ident :: ReadP String
ident = many1 (satisfy isAlphaNum)

initial :: ReadP Initial
initial = do (,) <$> many1 (satisfy isAlphaNum) <*> (string ": " *> (read . (:"") <$> satisfy isDigit))

gate :: ReadP (String, Gate)
gate = do
    n1 <- ident
    string " "
    op <- choice (map string ["XOR","OR","AND"])
    string " "
    n2 <- ident
    string " -> "
    n3 <- ident
    return (n3,(op,(n1,n2)))

spec :: ReadP ([String], Circuit, Values)
spec = do
    inits <- sepBy1 initial (string "\n")
    string "\n\n"
    gates <- sepBy1 gate (string "\n")
    let gather (n1,(_,(n2,n3))) = [n1,n2,n3]
        nodes = sort $ nub $ concatMap gather gates
        circuit = M.fromList gates
        values = M.fromList inits
    return (nodes, circuit, values)

compute :: Circuit -> Values -> String -> Int
compute c v node
    | node `M.member` v = fromJust $ M.lookup node v
    | node `M.member` c = apply op (compute c v a) (compute c v b)
    | otherwise = 0
        where (op,(a,b)) = fromJust $ M.lookup node c
              apply "XOR" a b = a `xor` b
              apply "AND" a b = a .&. b
              apply "OR" a b = a .|. b

partOne :: [String] -> Circuit -> Values -> Int
partOne nodes circuit values = foldl (\a b -> 2*a + b) 0 (reverse vs)
    where zs = filter ((=='z').head) nodes
          vs = map (compute circuit values) zs

tryAdding :: [String] -> Circuit -> Int -> Int -> Int
tryAdding nodes circuit x y = partOne nodes circuit (M.fromList $ xs ++ ys)
    where xs = zip ["x" ++ drop 1 (show (100+n)) | n <- [0..45]] (toBinary x)
          ys = zip ["y" ++ drop 1 (show (100+n)) | n <- [0..45]] (toBinary y)

toBinary :: Int -> [Int]
toBinary = unfoldr toBinary'
    where toBinary' 0 = Nothing
          toBinary' n = Just (n `mod` 2, n `div` 2)

main = do
    (nodes, circuit, values) <- fromJust . parseMaybe spec <$> readFile "day24.txt"
    -- print $ partOne nodes circuit values
    let adderLines (target,(op,(n1,n2))) = [concat [n1," -> ",target," [label=\""++op++"\"];"], concat [n2," -> ",target," [label=\""++op++"\"];"]]
    mapM_ putStrLn $ concatMap adderLines $ M.assocs circuit

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
