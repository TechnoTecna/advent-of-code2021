module AoC01
  ( solve01 )
  where

import Data.List (intercalate) -- for tests


-- Part 1
f1 :: [Int] -> Int
f1 l = sum $ map fromEnum $ zipWith (<) l (tail l)

-- Part 2
f2 :: [Int] -> Int
f2 l = f1 $ map (\(a, b, c) -> a + b + c) $ zip3 l (tail l) (tail $ tail l)

-- Main
rawToInput :: String -> [Int]
rawToInput = map read . lines

solve01 :: String -> (Int, Int)
solve01 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput1"
  return $ solve01 raw

test :: (Int, Int)
test = solve01 rawTest

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

inputTest :: [Int]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 1832
res2 :: Int
res2 = 1858
