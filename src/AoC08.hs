module AoC08
  ( solve08 )
  where

import Data.Bifunctor (second)
import Data.List (intercalate) -- for test
import Data.List (sortBy , groupBy, sort, (\\), find, nub, elemIndex)
import Data.Maybe (fromJust)
import qualified Utils as U


-- Part 1
segs :: [String]
segs = map sort [ "abcefg", "cf", "acdeg", "acdfg", "bcdf"
                , "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" ]

segNb :: [(Int, [Int])]
segNb = map (\((i,l):t) -> (l, i : map fst t))
        $ groupBy (\a b -> snd a == snd b)
        $ sortBy (\a b -> snd a `compare` snd b)
        $ zip [0..]
        $ map length segs

f1 :: [([String], [String])] -> Int
f1 = length
     . filter ((==) 1 . length)
     . map (fromJust . flip lookup segNb . length)
     . concatMap snd

-- Part 2
deduce :: [String] -> (Char, [(Char, Char)])
deduce nbs = (head a, map U.first2 [bd, cf, bd, eg, cf, eg])
  where one   = fromJust $ find ((==) 2 . length) nbs
        four  = fromJust $ find ((==) 4 . length) nbs
        seven = fromJust $ find ((==) 3 . length) nbs
        eight = fromJust $ find ((==) 7 . length) nbs
        cf = one
        a  = seven \\ cf
        bd = four \\ cf
        eg = (eight \\ four) \\ a

trans :: [String] -> String -> [String]
trans nbs shf = map (map (fromJust . flip lookup table)) nbs
  where table = zip shf "abcdefg"

check :: [String] -> String -> Bool
check nbs shf =
  length cShf == 7 && sort segs == sort (map sort (trans nbs cShf))
  where cShf = nub shf

checkAll :: String -> [(Char, Char)] -> [String] -> String -- 64 ~= 8
checkAll acc [] nbs
  | check nbs acc = acc
  | otherwise     = ""
checkAll acc ((a, b):t) nbs =
  checkAll (acc ++ [a]) t nbs ++ checkAll (acc ++ [b]) t nbs

f2 :: [([String], [String])] -> Int
f2 l = sum $ map (sum . zipWith (*) [10^n | n <- [0..]] . reverse
                  . map (fromJust . flip elemIndex segs . sort)
                  . (\(dig, out) -> trans out
                                    $ (\(a, t) -> checkAll [a] t dig)
                                    $ deduce dig))
       l

-- Main
rawToInput :: String -> [([String], [String])]
rawToInput = map (U.first2 . map (U.splitWhen (== ' ')) . U.splitWhen (== '|'))
             . lines

solve08 :: String -> (Int, Int)
solve08 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput8"
  return $ solve08 raw

test :: (Int, Int)
test = solve08 rawTest

linesTest :: [String]
linesTest =
  [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
  , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
  , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
  , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
  , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
  , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
  , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
  , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
  , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
  , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [([String], [String])]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 493
res2 :: Int
res2 = 1010460
