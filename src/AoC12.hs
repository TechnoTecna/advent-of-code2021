module AoC12
  ( solve12 )
  where

import Data.Char (isLower)
import Data.List (intercalate) -- for test
import qualified Utils as U
import Data.Maybe (fromJust)


-- Part 1
exit :: [(String, String)] -> U.Map String [String]
exit = foldr (\(s, e) m -> U.setWith (U.setWith m (s:) e [s]) (e:) s [e]) U.Leaf

paths :: U.Map String [String] -> [String] -> String -> [[String]]
paths _ acc "end" = ["end" : acc]
paths m acc cur
  | isLower (head cur) && cur `elem` acc = []
  | otherwise = concatMap (paths m (cur:acc)) $ fromJust $ U.get m cur

f1 :: [(String, String)] -> Int
f1 eds = length $ paths (exit eds) [] "start"

-- Part 2
paths2 :: Bool -> U.Map String [String] -> [String] -> String -> [[String]]
paths2 _ _ acc "end" = ["end" : acc]
paths2 b m acc cur
  | cur == "end" || (isLower (head cur) && cur `elem` acc && b) = []
  | cur == "start" && cur `elem` acc = []
  | isLower (head cur) && cur `elem` acc
    && not b = concatMap (paths2 True m (cur:acc)) $ fromJust $ U.get m cur
  | otherwise = concatMap (paths2 b m (cur:acc)) $ fromJust $ U.get m cur

f2 :: [(String, String)] -> Int
f2 eds = length $ paths2 False (exit eds) [] "start"

-- Main
rawToInput :: String -> [(String, String)]
rawToInput = map (U.first2 . U.splitOnList "-") . lines

solve12 :: String -> (Int, Int)
solve12 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput12"
  return $ solve12 raw

test :: (Int, Int)
test = solve12 rawTest

linesTest :: [String]
linesTest =
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [(String, String)]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 4659
res2 :: Int
res2 = 148962
