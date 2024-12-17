module Day17 where

import Data.Bits (xor)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Char (isDigit)
type Regs = M.Map Int Int

combo :: Regs -> Int -> Int
combo regs val | val <= 3 = val
               | otherwise = M.findWithDefault 0 (val-4) regs

execute :: (Int,Regs,[Int]) -> Int -> Int -> (Int,Regs,[Int])
execute (pc,regs, out) op arg
    | op == 0 = (pc+2, M.insert 0 (reg 0 `div` (2 ^ combo regs arg)) regs, out)
    | op == 1 = (pc+2, M.insert 1 (reg 1 `xor` arg) regs, out)
    | op == 2 = (pc+2, M.insert 1 (combo regs arg `mod` 8) regs, out)
    | op == 3 = (if reg 0 == 0 then pc+2 else arg, regs, out)
    | op == 4 = (pc+2, M.insert 1 (reg 1 `xor` reg 2) regs, out)
    | op == 5 = (pc+2, regs, combo regs arg `mod` 8 : out)
    | op == 6 = (pc+2, M.insert 1 (reg 0 `div` (2 ^ combo regs arg)) regs, out)
    | op == 7 = (pc+2, M.insert 2 (reg 0 `div` (2 ^ combo regs arg)) regs, out)
    | otherwise = error "Wrong opcode"
  where reg n = M.findWithDefault 0 n regs

step :: [Int] -> (Int,Regs,[Int]) -> (Int,Regs,[Int])
step program (pc,regs, out)
   | pc > length program - 2 = (pc,regs, out)
   | otherwise = execute (pc,regs,out) (program !! pc) (program !! (pc + 1))

run :: [Int] -> Int -> (Int,Regs,[Int])
run program aVal = last $ takeWhile pcGood $ iterate (step program) start
    where start = (0, M.insert 0 aVal M.empty, [])
          pcGood (pc,_,_) = pc <= length program - 2

output :: (Int,Regs,[Int]) -> String
output (_,_,out) = intercalate "," $ map show $ reverse out

parse :: [String] -> ([Int],Int)
parse lines = (program, value)
    where value = read $ filter isDigit $ head lines
          program = map read $ words $ map (\c -> if isDigit c then c else ' ') $ lines !! 4

partOne :: [Int] -> Int -> String
partOne program value = output $ run program value

main = do
    (program, value) <- parse . lines <$> readFile "day17.txt"
    print $ partOne program value