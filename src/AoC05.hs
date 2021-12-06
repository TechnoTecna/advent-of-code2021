module AoC05
  ( solve05 )
  where

import Data.Bifunctor (bimap)
import Data.List (intercalate) -- for test
import qualified Utils as U


-- Part 1
type Pos = (Int, Int)
type Line = (Pos, Pos)
type Grid = [[Int]]

maxPos :: Pos -> Pos -> Pos
maxPos (x1, y1) (x2, y2) = (max x1 x2, max y1 y2)

setGrid :: Grid -> (Int -> Int) -> Pos -> Grid
setGrid grd f (x, y) = take x grd
                       ++ [take y (grd !! x)
                           ++ [f $ grd !! x !! y]
                           ++ drop (y + 1) (grd !! x)]
                       ++ drop (x + 1) grd

filterDiag :: [Line] -> [Line]
filterDiag = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

updateGrid :: Grid -> Line -> Grid
updateGrid grd (s, e) =
  if s == e
  then newGrd
  else
    updateGrid newGrd (U.vecSum s (bimap U.dumbNorm U.dumbNorm $ U.vecDiff e s)
                      , e)
  where newGrd = setGrid grd (+ 1) s

f1 :: [Line] -> Int
f1 = f2 . filterDiag

-- Part 2
f2 :: [Line] -> Int
f2 ls = U.count (> 1) $ concat $ foldr (flip updateGrid) initG ls
  where initG = replicate (fst mPos + 1) $ replicate (snd mPos + 1) 0
        mPos = foldr1 maxPos $ map (uncurry maxPos) ls

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
