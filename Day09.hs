module Day09 where

import qualified Data.Sequence as S
import Data.Foldable
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Data.List.HT (takeUntil)

data Block = Free Int | File Int Int deriving (Eq, Show)
type Drive = S.Seq Block

parse :: [Int] -> Drive
parse = S.fromList . createBlock False 0
    where createBlock _ _ [] = []
          createBlock False id (len:lens) = File id len : createBlock True (id+1) lens
          createBlock True id (len:lens) = Free len : createBlock False id lens

defrag :: Drive -> Drive
defrag drive = d $ last $ takeUntil done $ iterate defragStep (1, S.length drive - 1, drive, False)
    where done (_,_,_,x) = x
          d (_,_,dr,_) = dr          

defragStep :: (Int,Int,Drive,Bool) -> (Int,Int,Drive,Bool)
defragStep (firstFree, lastFile, drive, done)
    | nothingToDo firstFreeBlock || firstFree >= lastFile = (firstFree, lastFile, drive, True)
    | fileLen > freeLen = (firstFree+2, lastFile, partiallyMoveLastFile, False)
    | freeLen > fileLen = (firstFree+1, lastFile-2+1, partiallyFillFirstFree, False)
    | freeLen == fileLen = (firstFree+2, lastFile-2, swapFreeAndFile, False)
      where firstFreeBlock = S.lookup firstFree drive
            lastFileBlock = S.lookup lastFile drive
            nothingToDo Nothing = True
            nothingToDo (Just (File _ _)) = True
            nothingToDo (Just (Free _)) = False
            (Just (Free freeLen)) = firstFreeBlock
            (Just (File id fileLen)) = lastFileBlock
            partiallyMoveLastFile = S.update firstFree (File id freeLen) shortenLastFile
            shortenLastFile = S.update lastFile (File id (fileLen-freeLen)) drive
            partiallyFillFirstFree = S.update firstFree (File id fileLen) shortenFree
            shortenFree = S.insertAt (firstFree+1) (Free (freeLen-fileLen)) freeLastFile
            freeLastFile = S.update lastFile (Free fileLen) drive
            swapFreeAndFile = S.update firstFree (fromJust $ S.lookup lastFile drive) swapFileAndFree
            swapFileAndFree = S.update lastFile (fromJust $ S.lookup firstFree drive) drive

checksum :: Drive -> Int
checksum = doCS 0 0 . toList
    where doCS count total [] = total
          doCS count total (File id len:rest) = doCS (count+len) (total+blockCS) rest
            where blockCS = sum $ map (id *) $ take len [count..]
          doCS count total (Free len:rest) = doCS (count+len) total rest

partOne :: Drive -> Int
partOne = checksum . defrag

main = do
    driveMap <- map digitToInt <$> readFile "day09.txt"
    print $ partOne $ parse driveMap