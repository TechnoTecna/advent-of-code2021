-- TODO: set module name with day number
module AoCXX
  -- TODO: set solve function name with day number
  ( solveXX )
  where

import Data.List (intercalate) -- for test


-- Part 1
-- TODO: set f1 type
f1 = undefined

-- Part 2
-- TODO: set f2 type
f2 = undefined

-- Main
-- TODO: set rawToInput type
rawToInput = undefined

-- TODO: set solve name with day number
solveXX :: String -> (Int, Int)
solveXX raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  -- TODO: set file name with current day number
  raw <- readFile "data/AoCInputXX"
  -- TODO: set solve name with day number
  return $ solveXX raw

test :: (Int, Int)
-- TODO: set solve name with day number
test = solveXX rawTest

linesTest :: [String]
-- TODO: copy example into linesTest
linesTest = []

rawTest :: String
rawTest = intercalate "\n" linesTest

-- TODO: set inputTest type
inputTest = rawToInput rawTest

res1 :: Int
-- TODO: record result of part 1
res1 = undefined
res2 :: Int
-- TODO: record result of part 2
res2 = undefined
