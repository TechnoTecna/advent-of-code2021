module AoC11
  ( solve11 )
  where

import Data.Bifunctor (first, bimap)
import Data.List (intercalate) -- for test
import Control.Applicative ((<*>))
import qualified Utils as U


-- Part 1
flashs :: [[Int]] -> [[Int]]
flashs = map (map (fromEnum . (<) 9))

flashLine :: [Int] -> [Int]
flashLine = zipWith (+) . (flip (++) [0] . tail)
            <*> (zipWith (+) <*> (take 10 . (:) 0))

flashMap :: [[Int]] -> [[Int]]
flashMap =
  (zipWith (zipWith (+)) . (flip (++) [replicate 10 0] . tail)
  <*> (zipWith (zipWith (+)) <*> (take 10 . (:) (replicate 10 0))))
  . map flashLine

nullMask :: [[Int]] -> Bool
nullMask = all (all (0 ==))

apply :: [[Int]] -> [[Int]] -> [[Int]]
apply = zipWith (zipWith (+))

toDark :: [[Int]] -> (Int, [[Int]])
toDark g = if nullMask fs then (0, g) else first (nf +) $ toDark ng
    where ng = apply (flashMap fs) $ map (map (\x -> if x>9 then -101 else x)) g
          fs = flashs g
          nf = sum $ concat fs

step :: (Int, [[Int]]) -> (Int, [[Int]])
step (i, g) = bimap (i +)
                    (map (map (\x -> if x>9 || x<0 then 0 else x)))
                    (toDark ng)
  where ng = apply (replicate 10 $ replicate 10 1) g

someSteps :: Int -> [[Int]] -> (Int, [[Int]])
someSteps i g = foldl (\x _ -> step x) (0, g) [1..i]

f1 :: [[Int]] -> Int
f1 = fst . someSteps 100

-- Part 2
untilDawn :: Int -> [[Int]] -> Int
untilDawn i g = if fs == 100 then i else untilDawn (i+1) ng
  where (fs, ng) = step (0, g)

f2 :: [[Int]] -> Int
f2 = untilDawn 1

-- Main
rawToInput :: String -> [[Int]]
rawToInput = map (map (read . flip (:) [])) . lines

solve11 :: String -> (Int, Int)
solve11 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput11"
  return $ solve11 raw

test :: (Int, Int)
test = solve11 rawTest

linesTest :: [String]
linesTest =
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [[Int]]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 1669
res2 :: Int
res2 = 351
