module Day09 where

import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Foldable
import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust)
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
          doCS count total (File fileId len:rest) = doCS (count+len) (total+blockCS) rest
            where blockCS = sum $ map (fileId *) $ take len [count..]
          doCS count total (Free len:rest) = doCS (count+len) total rest

partOne :: Drive -> Int
partOne = checksum . defrag

defragTwo :: Drive -> Drive
defragTwo drive = snd $ foldl defragStepTwo (locs, drive) (reverse fileIds)
  where locs = M.fromList $ [(id,id*2) | id <- fileIds]
        fileIds = [0..S.length drive `div` 2]
        fileLen (Just (File _ len)) = len

defragStepTwo :: (M.Map Int Int, Drive) -> Int -> (M.Map Int Int, Drive)
defragStepTwo (locs, drive) fileId = if canDefrag then fit else (locs, drive)
  where canDefrag = isJust firstFreeSpace && fromJust firstFreeSpace < fileLoc
        firstFreeSpace = S.findIndexL isFreeAndEnough drive
        isFreeAndEnough (File _ _) = False
        isFreeAndEnough (Free freeLen) = freeLen >= fileLen
        Just (Free freeLen) = S.lookup (fromJust firstFreeSpace) drive
        fileLoc = fromJust $ M.lookup fileId locs
        (Just (File _ fileLen)) = S.lookup fileLoc drive
        fit = if freeLen == fileLen then (locs, swapFreeAndFile) else moveFileUp
        swapFreeAndFile = S.update (fromJust firstFreeSpace) (File fileId fileLen) preSwap
        preSwap = S.update fileLoc (Free fileLen) drive
        moveFileUp = (shiftLocs, partiallyFill)
        shiftLocs = M.map (\i -> if i > fromJust firstFreeSpace then i+1 else i) locs
        partiallyFill = S.update (fromJust firstFreeSpace) (File fileId fileLen) shortenFree
        shortenFree = S.insertAt (fromJust firstFreeSpace+1) (Free (freeLen-fileLen)) freeLastFile
        freeLastFile = S.update fileLoc (Free fileLen) drive

partTwo :: Drive -> Int
partTwo drive = checksum $ defragTwo drive

main = do
    driveMap <- map digitToInt <$> readFile "day09.txt"
    print $ partOne $ parse driveMap
    print $ partTwo $ parse driveMap