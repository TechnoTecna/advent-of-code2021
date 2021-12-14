module AoC04
  ( solve )
  where

import Data.List (isSubsequenceOf, sort, transpose, (\\), find)
import Data.List (intercalate) -- for test
import Data.Maybe (fromJust)
import qualified Utils as U

type Result1 = Int
type Result2 = Int
type Input = ([Int], [[[Int]]])


-- Part1
isSubset :: Ord a => [a] -> [a] -> Bool
isSubset ll = isSubsequenceOf (sort ll) . sort

dropWhileInc :: ([a] -> Bool) -> [a] -> [a]
dropWhileInc _ [] = []
dropWhileInc p (h:t)
  | p t = dropWhileInc p t
  | otherwise = h:t

won :: [Int] -> [[Int]] -> Bool
won drw brd = any (`isSubset` drw) brd || any (`isSubset` drw) (transpose brd)

minDraw :: [[[Int]]] -> [Int] -> [Int]
minDraw brds = reverse . dropWhileInc (flip any brds . won) . reverse

sumUnmarked :: [Int] -> [[Int]] -> Int
sumUnmarked drw = sum . (\\ drw) . concat

f1 :: Input -> Result1
f1 (drw, brds) = last mdrw * sumUnmarked mdrw (fromJust $ find (won mdrw) brds)
              where mdrw = minDraw brds drw

-- Part 2
maxDraw :: [[[Int]]] -> [Int] -> [Int]
maxDraw brds = reverse . dropWhileInc (flip all brds . won) . reverse

f2 :: Input -> Result2
f2 (drw, brds) =
  last mdrw
  * sumUnmarked mdrw (fromJust $ find (not . won (tail $ reverse mdrw)) brds)
    where mdrw = maxDraw brds drw

-- Main
rawToInput :: String -> Input
rawToInput i =
  (map read $ U.splitWhen (== ',') $ head $ lines i
  , map (map (map read . words)) $ U.splitWhen (=="") $ drop 2 $ lines i)

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput4"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest =
  [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
  , ""
  , "22 13 17 11  0"
  , " 8  2 23  4 24"
  , "21  9 14 16  7"
  , " 6 10  3 18  5"
  , " 1 12 20 15 19"
  , ""
  , " 3 15  0  2 22"
  , " 9 18 13 17  5"
  , "19  8  7 25 23"
  , "20 11 10 24  4"
  , "14 21 16 12  6"
  , ""
  , "14 21 17 24  4"
  , "10 16 15  9 19"
  , "18  8 23 26 20"
  , "22 11 13  6  5"
  , " 2  0 12  3  7"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

board1Test :: [[Int]]
board1Test = snd inputTest !! 0
board2Test :: [[Int]]
board2Test = snd inputTest !! 1
board3Test :: [[Int]]
board3Test = snd inputTest !! 2

res1 :: Result1
res1 = 6592
res2 :: Result2
res2 = 31755
