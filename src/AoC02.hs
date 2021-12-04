module AoC02
  ( solve02 )
  where

import Data.Bifunctor (second)


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
linesToInput :: [String] -> [(String, Int)]
linesToInput = map (second (read . tail) . break (==' '))

solve02 :: FilePath -> IO (Int, Int)
solve02 fp = do
  raw <- readFile fp
  let input = linesToInput $ lines raw
  return (f1 input, f2 input)



-- Tests
testLines =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ]

test = linesToInput testLines

file = "../data/input-02.txt"

res1 = 1882980
res2 = 1971232560
