module AoC02
  ( solve )
  where

import Data.Bifunctor (second)
import Data.List (intercalate) -- for tests

type Result1 = Int
type Result2 = Int
type Input = [(String, Int)]


-- Part 1
toMove :: (String, Int) -> (Int, Int)
toMove ('f':_, x) = (x, 0)
toMove ('u':_, x) = (0, -x)
toMove ('d':_, x) = (0, x)

f1 :: Input -> Result1
f1 = uncurry (*)
     . foldr1 (\(a, b) (c, d) -> (a+c, b+d))
     . map toMove

-- Part 2
f2 :: Input -> Result2
f2 = uncurry (*)
     . foldl1 (\(a, b) (c, d) -> (a+c, c*d+b))
     . scanl1 (\(_, b) (c, d) -> (c, b+d))
     . map toMove

-- Main
rawToInput :: String -> Input
rawToInput = map (second (read . tail) . break (==' ')) . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput2"
  return $ solve raw

test :: (String, String)
test = solve rawTest

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

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 1882980
res2 :: Result2
res2 = 1971232560
