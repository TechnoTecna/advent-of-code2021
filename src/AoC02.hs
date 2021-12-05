module AoC02
  ( solve02 )
  where

import Data.Bifunctor (second)
import Data.List (intercalate) -- for tests


-- Part 1
toMove :: (String, Int) -> (Int, Int)
toMove ('f':_, x) = (x, 0)
toMove ('u':_, x) = (0, -x)
toMove ('d':_, x) = (0, x)

f1 :: [(String, Int)] -> Int
f1 = uncurry (*)
     . foldr1 (\(a, b) (c, d) -> (a+c, b+d))
     . map toMove

-- Part 2
f2 :: [(String, Int)] -> Int
f2 = uncurry (*)
     . foldl1 (\(a, b) (c, d) -> (a+c, c*d+b))
     . scanl1 (\(_, b) (c, d) -> (c, b+d))
     . map toMove

-- Main
rawToInput :: String -> [(String, Int)]
rawToInput = map (second (read . tail) . break (==' ')) . lines

solve02 :: String -> (Int, Int)
solve02 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput2"
  return $ solve02 raw

test :: (Int, Int)
test = solve02 rawTest

linesTest :: [String]
linesTest =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [(String, Int)]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 1882980
res2 :: Int
res2 = 1971232560
