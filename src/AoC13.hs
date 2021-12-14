module AoC13
  ( solve )
  where

import Data.Bifunctor (bimap)
import Data.List (intercalate) -- for test
import Data.List (nub)
import qualified Utils as U

type Result1 = Int
type Result2 = String
type Input = ([(Int, Int)], [(Bool, Int)])


-- Part 1
foldPoint :: (Bool, Int) -> (Int, Int) -> (Int, Int)
foldPoint (h, f) (x, y)
  | h     && f < y = (x,           y-(2*(y-f)))
  | not h && f < x = (x-(2*(x-f)), y)
  | otherwise      = (x, y)

foldPaper :: [(Int, Int)] -> [(Bool, Int)] -> [(Int, Int)]
foldPaper ps fs = nub $ foldl (\b a -> map (foldPoint a) b) ps fs

f1 :: Input -> Result1
f1 (ps, f:_) = length $ foldPaper ps [f]

-- Part 2
toGrid :: [(Int, Int)] -> [[Bool]]
toGrid ps = map (\y -> map (\x -> (x, y) `elem` ps) [0..maxX]) [0..maxY]
  where maxX = maximum $ map fst ps
        maxY = maximum $ map snd ps

printGrid :: [[Bool]] -> String
printGrid = concatMap (flip (++) "\n" . map (\b -> if b then '#' else '.'))

f2 :: Input -> Result2
f2 (ps, fs) = printGrid $ toGrid $ foldPaper ps fs

-- Main
rawToInput :: String -> Input
rawToInput =
  bimap (map (U.first2 . map read . U.splitWhen (== ',')))
        (map (bimap ((== 'y') . last) read . U.first2 . U.splitWhen (== '=')))
  . U.first2 . U.splitWhen (== "") . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput13"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest =
  [ "6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along y=7"
  , "fold along x=5"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 788
res2 :: Result2
res2 = "#..#...##.###..#..#.####.#..#.###...##."
    ++ "#.#.....#.#..#.#.#..#....#..#.#..#.#..#"
    ++ "##......#.###..##...###..#..#.###..#..."
    ++ "#.#.....#.#..#.#.#..#....#..#.#..#.#.##"
    ++ "#.#..#..#.#..#.#.#..#....#..#.#..#.#..#"
    ++ "#..#..##..###..#..#.####..##..###...###"
