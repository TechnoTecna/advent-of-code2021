module AoC03
  ( solve03 )
  where

import Data.Char (digitToInt)
import Data.List (intercalate) -- for test


-- Part1
binToInt :: [Bool] -> Int
binToInt = sum . zipWith (*) [2^n | n <- [0..]] . reverse . map fromEnum

gam :: [[Bool]] -> [Bool]
gam l = map (half <=) $ foldr1 (zipWith (+)) $ map (map fromEnum) l
        where half = (length l `div` 2) + (length l `mod` 2)

f1 :: [[Bool]] -> Int
f1 l = binToInt (gam l) * binToInt (map not $ gam l)

-- Part 2
recF :: ([[Bool]] -> Bool) -> [[Bool]] -> [Bool]
recF _ [a] = a
recF f l   = f l : recF f (map tail $ filter ((== f l) . head) l)

f2 :: [[Bool]] -> Int
f2 l = binToInt (recF (head . gam) l) * binToInt (recF (not . head . gam) l)

-- Main
rawToInput :: String -> [[Bool]]
rawToInput = map (map ((==) 1 . digitToInt)) . lines

solve03 :: String -> (Int, Int)
solve03 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput3"
  return $ solve03 raw

test :: (Int, Int)
test = solve03 rawTest

linesTest :: [String]
linesTest =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [[Bool]]
inputTest = rawToInput rawTest

printL :: Show a => [a] -> IO ()
printL l = do
  let strLn = map show l
  let res = foldr1 ((++) . flip (++) "\n") strLn
  putStrLn res

res1 :: Int
res1 = 3374136
res2 :: Int
res2 = 4432698
