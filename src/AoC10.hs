module AoC10
  ( solve )
  where

import Data.List (intercalate) -- for test
import Data.List (sort)

type Result1 = Int
type Result2 = Int
type Input = [String]


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

f1 :: Input -> Result1
f1 = sum . map (score1 . fst . badChar [])

-- Part 2
score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4

middle :: [a] -> a
middle l = l !! (length l `div` 2)

f2 :: Input -> Result2
f2 = middle
     . sort
     . map (foldl (\a b -> score2 b + (a * 5)) 0 . snd)
     . filter ((==) '0' . fst)
     . map (badChar [])

-- Main
rawToInput :: String -> Input
rawToInput = lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput10"
  return $ solve raw

test :: (String, String)
test = solve rawTest

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

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 288291
res2 :: Result2
res2 = 820045242
