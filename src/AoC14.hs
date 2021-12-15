module AoC14
  ( solve )
  where

import Data.Bifunctor (bimap)
import Data.List (intercalate) -- for test
import Data.List (group, sort)
import Data.Maybe (fromJust)
import qualified Utils as U

type Result1 = Int
type Result2 = Int
type Input = (String, U.Map (Char, Char) Char)


-- Part 1
expandPair :: U.Map (Char, Char) Char
           -> U.Map (Char, Char) Int
           -> U.Map (Char, Char) Int
           -> U.Map Char Int
           -> (Char, Char)
  -> (U.Map (Char, Char) Int, U.Map Char Int)
expandPair m opm pm cm p@(a, c) =
  case U.get m p of
    Nothing -> (U.setWith' pm (+ nbP) p, cm)
    Just b  -> (U.setWith' (U.setWith' pm (+ nbP) (a, b)) (+ nbP) (b, c)
               ,U.setWith cm (+ nbP) b nbP)
  where nbP = fromJust $ U.get opm p

expandPairs :: U.Map (Char, Char) Char
            -> U.Map (Char, Char) Int
            -> U.Map Char Int
  -> (U.Map (Char, Char) Int, U.Map Char Int)
expandPairs m opm cm =
  foldr (\a (pm, cm) -> expandPair m opm pm cm a) (bpm, cm) $ U.keys m
  where bpm = U.mapMap (const 0) opm

steps :: Int
      -> U.Map (Char, Char) Char
      -> U.Map (Char, Char) Int
      -> U.Map Char Int
  -> (U.Map (Char, Char) Int, U.Map Char Int)
steps i m pm cm = foldl (\(pm, cm) a -> expandPairs m pm cm) (pm, cm) [1..i]

initMaps :: String -> U.Map (Char, Char) Char
  -> (U.Map (Char, Char) Int, U.Map Char Int)
initMaps s m = (bpm, bcm)
  where bpm = foldr (\a b -> U.setWith' b (+ 1) a) (U.mapMap (const 0) m)
              $ zip s (tail s)
        bcm = foldr (\a b -> U.setWith b (+ 1) a 1) U.Leaf s

f1 :: Input -> Result1
f1 (s, m) = (\l -> maximum l - minimum l) $ U.values $ snd
            $ uncurry (steps 10 m) $ initMaps s m

-- Part 2
f2 :: Input -> Result2
f2 (s, m) = (\l -> maximum l - minimum l) $ U.values $ snd
            $ uncurry (steps 40 m) $ initMaps s m

-- Main
rawToInput :: String -> Input
rawToInput =
  bimap
    head
    (foldr (\a b -> uncurry (U.set b) $ ((a!!0, a!!1), last a)) U.Leaf . tail)
  . break (== "") . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput14"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest =
  [ "NNCB"
  , ""
  , "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 2233
res2 :: Result2
res2 = 2884513602164
