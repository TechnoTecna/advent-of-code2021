module AoC12
  ( solve )
  where

import Data.Char (isLower)
import Data.List (intercalate) -- for test
import qualified Utils as U
import Data.Maybe (fromJust)

type Result1 = Int
type Result2 = Int
type Input = [(String, String)]


-- Part 1
exit :: [(String, String)] -> U.Map String [String]
exit = foldr (\(s, e) m -> U.setWith (U.setWith m (s:) e [s]) (e:) s [e]) U.Leaf

paths :: U.Map String [String] -> [String] -> String -> [[String]]
paths _ acc "end" = ["end" : acc]
paths m acc cur
  | isLower (head cur) && cur `elem` acc = []
  | otherwise = concatMap (paths m (cur:acc)) $ fromJust $ U.get m cur

f1 :: Input -> Result1
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

f2 :: Input -> Result2
f2 eds = length $ paths2 False (exit eds) [] "start"

-- Main
rawToInput :: String -> Input
rawToInput = map (U.first2 . U.splitOnList "-") . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput12"
  return $ solve raw

test :: (String, String)
test = solve rawTest

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

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 4659
res2 :: Result2
res2 = 148962
