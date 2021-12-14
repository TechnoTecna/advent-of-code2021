module AoC06
  ( solve )
  where

import Data.List (group, sort)
import Data.List (intercalate) -- for test
import qualified Utils as U

type Result1 = Int
type Result2 = Int
type Input = [Int]


-- Part 1
initBatch :: [Int] -> [Int]
initBatch = map ((+) (-1) . length) . group . sort . (++) [0..8]

oneDay :: [Int] -> [Int]
oneDay (h:t) = take 6 t ++ [h + t !! 6] ++ [t !! 7] ++ [h]

someDays :: Int -> [Int] -> [Int]
someDays i l = foldl (\x _ -> oneDay x) (initBatch l) [1..i]

f1 :: Input -> Result1
f1 = sum . someDays 80

-- Part 2
f2 :: Input -> Result2
f2 = sum . someDays 256

-- Main
rawToInput :: String -> Input
rawToInput = map read . U.splitWhen (== ',')

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput6"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest = ["3,4,3,1,2"]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 380612
res2 :: Result2
res2 = 1710166656900
