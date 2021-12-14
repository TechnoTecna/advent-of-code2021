-- TODO: set module name with day number
module AoCXX
  ( solve )
  where

import Data.List (intercalate) -- for test

-- TODO: set result type for Part1
type Result1 = undefined
-- TODO: set result type for Part2
type Result2 = undefined
-- TODO: set input type
type Input = undefined


-- Part 1
f1 :: Input -> Result1
f1 = undefined

-- Part 2
f2 :: Input -> Result2
f2 = undefined

-- Main
rawToInput :: String -> Input
rawToInput = undefined

solve :: String -> (String, String)
-- TODO: remove show if Result is String
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  -- TODO: set file name with current day number
  raw <- readFile "data/AoCInputXX"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
-- TODO: copy example into linesTest
linesTest = []

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
-- TODO: record result of part 1
res1 = undefined
res2 :: Result2
-- TODO: record result of part 2
res2 = undefined
