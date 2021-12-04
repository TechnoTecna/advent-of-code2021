module AoC04
  ( solve04 )
  where

import Data.List (isSubsequenceOf, sort, transpose, (\\), find)
import Data.Maybe (fromJust)


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

f1 :: [Int] -> [[[Int]]] -> Int
f1 drw brds = last mdrw * sumUnmarked mdrw (fromJust $ find (won mdrw) brds)
              where mdrw = minDraw brds drw

-- Part 2
maxDraw :: [[[Int]]] -> [Int] -> [Int]
maxDraw brds = reverse . dropWhileInc (flip all brds . won) . reverse

f2 :: [Int] -> [[[Int]]] -> Int
f2 drw brds =
  last mdrw
  * sumUnmarked mdrw (fromJust $ find (not . won (tail $ reverse mdrw)) brds)
    where mdrw = maxDraw brds drw

-- Main
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
                  [] -> []
                  t  -> w : splitWhen p t2
                    where (w, t2) = break p t

linesToDraw :: [String] -> [Int]
linesToDraw = map read . splitWhen (== ',') . head

linesToBoards :: [String] -> [[[Int]]]
linesToBoards = map (map (map read . words)) . splitWhen (==""). drop 2

solve04 :: FilePath -> IO (Int, Int)
solve04 fp = do
  raw <- readFile fp
  let inLines = lines raw
  let draw = linesToDraw inLines
  let boards =  linesToBoards inLines
  return (f1 draw boards, f2 draw boards)



-- Tests
testLines =
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

testDraw = linesToDraw testLines
testBoards = linesToBoards testLines

file = "../data/AoCInput4"

testBoard1 = testBoards !! 0
testBoard2 = testBoards !! 1
testBoard3 = testBoards !! 2

res1 = 6592
res2 = 31755
