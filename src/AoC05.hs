module AoC05
  ( solve05 )
  where

import Data.Bifunctor (bimap)
import Data.List (sort, group)
import Data.List (intercalate) -- for test
import qualified Utils as U


-- Part 1
type Pos = (Int, Int)
type Line = (Pos, Pos)
type Grid = [[Int]]

filterDiag :: [Line] -> [Line]
filterDiag = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

trace :: Line -> [Pos]
trace (s, e) =
  if s == e
  then [s]
  else s : trace (U.vecSum s (bimap U.dumbNorm U.dumbNorm $ U.vecDiff e s), e)

f1 :: [Line] -> Int
f1 = f2 . filterDiag

-- Part 2
f2 :: [Line] -> Int
f2 = U.count ((<) 1 . length) . group . sort . concatMap trace

-- Main
rawToInput :: String -> [Line]
rawToInput = map ( bimap U.first2 U.first2
                 . U.first2
                 . map (map read . U.splitWhen (== ','))
                 . U.splitOnList " -> " )
             . lines

solve05 :: String -> (Int, Int)
solve05 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput5"
  return $ solve05 raw

test :: (Int, Int)
test = solve05 rawTest

linesTest :: [String]
linesTest =
  [ "0,9 -> 5,9"
  , "8,0 -> 0,8"
  , "9,4 -> 3,4"
  , "2,2 -> 2,1"
  , "7,0 -> 7,4"
  , "6,4 -> 2,0"
  , "0,9 -> 2,9"
  , "3,4 -> 1,4"
  , "0,0 -> 8,8"
  , "5,5 -> 8,2"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [Line]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 7142
res2 :: Int
res2 = 20012
