module AoC08
  ( solve )
  where

import Data.Bifunctor (second)
import Data.List (intercalate) -- for test
import Data.List (sort, (\\), find, nub, elemIndex, findIndices)
import Data.Maybe (fromJust)
import qualified Utils as U

type Result1 = Int
type Result2 = Int
type Input = [([String], [String])]


-- Part 1
segs :: [String]
segs = map sort [ "abcefg", "cf", "acdeg", "acdfg", "bcdf"
                , "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" ]

f1 :: Input -> Result1
f1 = length
     . filter ((==) 1 . length)
     . map (\l -> findIndices ((==) (length l) . length) segs)
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

f2 :: Input -> Result2
f2 l = sum $ map (sum . zipWith (*) [10^n | n <- [0..]] . reverse
                  . map (fromJust . flip elemIndex segs . sort)
                  . (\(dig, out) -> trans out
                                    $ (\(a, t) -> checkAll [a] t dig)
                                    $ deduce dig))
       l

-- Main
rawToInput :: String -> Input
rawToInput = map (U.first2 . map (U.splitWhen (== ' ')) . U.splitWhen (== '|'))
             . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput8"
  return $ solve raw

test :: (String, String)
test = solve rawTest

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

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 493
res2 :: Result2
res2 = 1010460
