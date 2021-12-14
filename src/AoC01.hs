module AoC01
  ( solve )
  where

import Data.List (intercalate) -- for tests

type Result1 = Int
type Result2 = Int
type Input = [Int]


-- Part 1
f1 :: Input -> Result1
f1 l = sum $ map fromEnum $ zipWith (<) l (tail l)

-- Part 2
f2 :: Input -> Result2
f2 l = f1 $ map (\(a, b, c) -> a + b + c) $ zip3 l (tail l) (tail $ tail l)

-- Main
rawToInput :: String -> Input
rawToInput = map read . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput1"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest =
  [ "199"
  , "200"
  , "208"
  , "210"
  , "200"
  , "207"
  , "240"
  , "269"
  , "260"
  , "263"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 1832
res2 :: Result2
res2 = 1858
