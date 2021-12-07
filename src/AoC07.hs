module AoC07
  ( solve07 )
  where

import Data.List ( group, sort, intercalate ) -- for test
import qualified Utils as U


-- Part 1
initBatch :: Int -> [Int] -> [Int]
initBatch max = map ((+) (-1) . length) . group . sort . (++) [0..max]

posToFuel :: (Int -> Int -> Int) -> [Int] -> [Int]
posToFuel c l = map (\i -> sum $ zipWith ((*) . c i) [0..] l) [0..length l - 1]

f1 :: [Int] -> Int
f1 l = minimum $ posToFuel (\i -> abs . (-) i) $ initBatch (maximum l) l

-- Part 2
f2 :: [Int] -> Int
f2 l = minimum $ posToFuel (\a b -> abs (b-a)*(abs (a-b)+1) `div` 2)
               $ initBatch (maximum l) l

-- Main
rawToInput :: String -> [Int]
rawToInput = map read . U.splitWhen (== ',')

solve07 :: String -> (Int, Int)
solve07 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput7"
  return $ solve07 raw

test :: (Int, Int)
test = solve07 rawTest

linesTest :: [String]
linesTest = ["16,1,2,0,4,2,7,1,2,14"]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [Int]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 359648
res2 :: Int
res2 = 100727924
