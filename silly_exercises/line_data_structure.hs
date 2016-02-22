{-

read a file and turn each line int a data structure that contains:
- first word
- last word
- length
- entire line

-}

import Data.List.Split (splitOn)
import Data.List (intercalate)

testInput :: [Char]
testInput = "this is the first line\nand this the second\nthere is a third"

data Line = Line [Char] [Char] [Char] Int
    deriving Show

splitOnSpace :: [Char] -> [[Char]]
splitOnSpace = splitOn " "

-- break up input into lines
lineList :: [[[Char]]]
lineList = map splitOnSpace $ lines testInput

makeLine :: [[Char]] -> Line
makeLine lineLI = Line (intercalate " " lineLI)
                       (head lineLI)
                       (last lineLI)
                       (length lineLI)

fancyLineList :: [Line]
fancyLineList = map makeLine lineList

getLineLength :: Line -> Int
getLineLength (Line _ _ _ len) = len

{-
$ fancyLineList
[Line "this is the first line" "this" "line" 5,
 Line "and this the second" "and" "second" 4,
 Line "there is a third" "there" "third" 4]

$ getLineLength $ head fancyLineList 
5
-}