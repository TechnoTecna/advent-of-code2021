module AoC05
  ( solve )
  where

import Data.Bifunctor (bimap)
import Data.List (sort, group)
import Data.List (intercalate) -- for test
import qualified Utils as U

type Result1 = Int
type Result2 = Int
type Input = [Line]


-- Part 1
type Pos = (Int, Int)
type Line = (Pos, Pos)

filterDiag :: [Line] -> [Line]
filterDiag = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

trace :: Line -> [Pos]
trace (s, e) =
  if s == e
  then [s]
  else s : trace (U.vecSum s (bimap U.dumbNorm U.dumbNorm $ U.vecDiff e s), e)

f1 :: Input -> Result1
f1 = f2 . filterDiag

-- Part 2
f2 :: Input -> Result2
f2 = U.count ((<) 1 . length) . group . sort . concatMap trace

-- Main
rawToInput :: String -> Input
rawToInput = map ( bimap U.first2 U.first2
                 . U.first2
                 . map (map read . U.splitWhen (== ','))
                 . U.splitOnList " -> " )
             . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput5"
  return $ solve raw

test :: (String, String)
test = solve rawTest

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

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 7142
res2 :: Result1
res2 = 20012
