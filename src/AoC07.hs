module AoC07
  ( solve )
  where

import Data.List ( group, sort, intercalate ) -- for test
import qualified Utils as U

type Result1 = Int
type Result2 = Int
type Input = [Int]


-- Part 1
initBatch :: Int -> [Int] -> [Int]
initBatch max = map ((+) (-1) . length) . group . sort . (++) [0..max]

posToFuel :: (Int -> Int -> Int) -> [Int] -> [Int]
posToFuel c l = map (\i -> sum $ zipWith ((*) . c i) [0..] l) [0..length l - 1]

f1 :: Input -> Result1
f1 l = minimum $ posToFuel (\i -> abs . (-) i) $ initBatch (maximum l) l

-- Part 2
f2 :: Input -> Result2
f2 l = minimum $ posToFuel (\a b -> abs (b-a)*(abs (a-b)+1) `div` 2)
               $ initBatch (maximum l) l

-- Main
rawToInput :: String -> Input
rawToInput = map read . U.splitWhen (== ',')

solve :: String -> (String, String)
solve raw = (show (f1 input), show(f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput7"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest = ["16,1,2,0,4,2,7,1,2,14"]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 359648
res2 :: Result2
res2 = 100727924
