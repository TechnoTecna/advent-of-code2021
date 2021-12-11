module AoC10
  ( solve10 )
  where

import Data.List (intercalate) -- for test
import Data.List (sort)


-- Part 1
score1 :: Char -> Int
score1 '0' = 0
score1 ')' = 3
score1 ']' = 57
score1 '}' = 1197
score1 '>' = 25137

badChar :: String -> String -> (Char, String)
badChar acc [] = ('0', acc)
badChar acc (i:u) =
  case (acc, i) of
    (')':t, ')') -> badChar t         u
    (']':t, ']') -> badChar t         u
    ('}':t, '}') -> badChar t         u
    ('>':t, '>') -> badChar t         u
    (_,     '(') -> badChar (')':acc) u
    (_,     '[') -> badChar (']':acc) u
    (_,     '{') -> badChar ('}':acc) u
    (_,     '<') -> badChar ('>':acc) u
    (_,     c)   -> (c, acc)

f1 :: [String] -> Int
f1 = sum . map (score1 . fst . badChar [])

-- Part 2
score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4

middle :: [a] -> a
middle l = l !! (length l `div` 2)

f2 :: [String] -> Int
f2 = middle
     . sort
     . map (foldl (\a b -> score2 b + (a * 5)) 0 . snd)
     . filter ((==) '0' . fst)
     . map (badChar [])

-- Main
rawToInput :: String -> [String]
rawToInput = lines

solve10 :: String -> (Int, Int)
solve10 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput10"
  return $ solve10 raw

test :: (Int, Int)
test = solve10 rawTest

linesTest :: [String]
linesTest =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: [String]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 288291
res2 :: Int
res2 = 820045242
