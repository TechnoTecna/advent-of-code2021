module AoC06
  ( solve06 )
  where

import Data.List (group, sort)
import Data.List (intercalate) -- for test
import qualified Utils as U


-- Part 1
initBatch :: [Int] -> [Int]
initBatch = map ((+) (-1) . length) . group . sort . (++) [0..8]

oneDay :: [Int] -> [Int]
oneDay (h:t) = take 6 t ++ [h + t !! 6] ++ [t !! 7] ++ [h]

someDays :: Int -> [Int] -> [Int]
someDays i l = foldl (\x _ -> oneDay x) (initBatch l) [1..i]

f1 :: [Int] -> Int
f1 = sum . someDays 80

-- Part 2
f2 :: [Int] -> Int
f2 = sum . someDays 256

-- Main
rawToInput :: String -> [Int]
rawToInput = map read . U.splitWhen (== ',')

solve06 :: String -> (Int, Int)
solve06 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput6"
  return $ solve06 raw

test :: (Int, Int)
test = solve06 rawTest

linesTest :: [String]
linesTest = ["3,4,3,1,2"]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [Int]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 380612
res2 :: Int
res2 = 1710166656900
