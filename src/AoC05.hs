module AoC05
  ( solve05 )
  where

import Data.Bifunctor (bimap)
import Data.List (intercalate) -- for test
import qualified Utils as U


-- Part 1
f1 :: [((Int, Int), (Int, Int))] -> Int
f1 = undefined

-- Part 2
f2 :: [((Int, Int), (Int, Int))] -> Int
f2 = undefined

-- Main
rawToInput :: String -> [((Int, Int), (Int, Int))]
rawToInput = map ( bimap U.first2 U.first2
                 . U.first2
                 . map (map read . U.splitWhen (== ','))
                 . U.splitOnList " -> " )
             . lines

solve05 :: String -> (Int, Int)
solve05 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput5"
  return $ solve05 raw

test :: (Int, Int)
test = solve05 rawTest

linesTest :: [String]
linesTest =
  [ "0,9 -> 5,9"
  , "8,0 -> 0,8"
  , "9,4 -> 3,4"
  , "2,2 -> 2,1"
  , "7,0 -> 7,4"
  , "6,4 -> 2,0"
  , "0,9 -> 2,9"
  , "3,4 -> 1,4"
  , "0,0 -> 8,8"
  , "5,5 -> 8,2"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [((Int, Int), (Int, Int))]
inputTest = rawToInput rawTest

res1 :: Int
-- TODO: record result of part 1
res1 = undefined
res2 :: Int
-- TODO: record result of part 2
res2 = undefined
