module AoC09
  ( solve )
  where

import Data.List (intercalate) -- for test
import Data.List (findIndices, transpose, groupBy, nub, intersect, sortBy)

type Result1 = Int
type Result2 = Int
type Input = [[Int]]


-- Part 1
lineLows :: [Int] -> [Int]
lineLows l = findIndices id
             $ zipWith3 (\p c n -> p > c && c < n) l' (tail l') (tail $ tail l')
  where l' = 9 : l ++ [9]

f1 :: Input -> Result1
f1 g = sum
       $ map (1 +)
       $ concat
       $ zipWith (\l is -> map (l !!) is) g
       $ zipWith (\i l -> filter (elem i . (!!) cLo) l) [0..] lLo
  where lLo = map lineLows g
        cLo = map lineLows $ transpose g

-- Part 2
data Tree a = Branch a [Tree a] deriving (Show)

instance (Eq a) => Eq (Tree a) where
  Branch x _ == Branch x' _ = x == x' -- Gotta go fast

non9 :: [[Int]] -> [[(Int, Int)]]
non9 = map (map (\s -> (fst $ head s, fst $ last s))
            . filter ((>) 9 . snd . head)
            . groupBy (\(_, a) (_, b) -> a < 9 && b < 9)
            . zip [0..])

fuseBranch :: Tree [(Int, Int)] -> Tree [(Int, Int)] -> Tree [(Int, Int)]
fuseBranch (Branch a s1) (Branch b s2) = Branch (a ++ b) $ nub (s1 ++ s2)

insertTree :: [Tree [(Int, Int)]] -> Tree [(Int, Int)] -> [Tree [(Int, Int)]]
insertTree [] n = [n]
insertTree (h@(Branch _ l):t) n@(Branch _ ln)
  | intersect l ln /= [] = insertTree t $ fuseBranch h n
  | otherwise            = h : insertTree t n

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (f, l) (g, m) = not $ (f < g && l < g) || (m < f && m < l)

inherit :: [Tree [(Int, Int)]] -> [(Int, Int)]
           -> ([Tree [(Int, Int)]], [Tree [(Int, Int)]])
inherit tr ln = (fn, foldl insertTree [] nw)
  where
    fn = filter (\s -> not $ any (\(Branch _ b) -> s `elem` b) nw) tr
    nw = map (\s -> Branch [s] $ filter (\(Branch x _) -> any (overlap s) x) tr)
             ln

toTree :: [Tree [(Int, Int)]] -> [[(Int, Int)]] -> [Tree [(Int, Int)]]
toTree lst []    = lst
toTree lst (h:t) = fn ++ toTree nw t
  where (fn, nw) = inherit lst h

size :: Tree [(Int, Int)] -> Int
size (Branch s b) = sum (map (\(f, l) -> l - f + 1) s) + sum (map size b)

f2 :: Input -> Result2
f2 = product . take 3 . sortBy (flip compare)  . map size . toTree [] . non9

-- Main
rawToInput :: String -> Input
rawToInput = map (map (read . flip (:) [])) . lines

solve :: String -> (String, String)
solve raw = (show (f1 input), show (f2 input))
  where input = rawToInput raw



-- Tests
run :: IO (String, String)
run = do
  raw <- readFile "data/AoCInput9"
  return $ solve raw

test :: (String, String)
test = solve rawTest

linesTest :: [String]
linesTest =
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]

rawTest :: String
rawTest = intercalate "\n" linesTest

inputTest :: Input
inputTest = rawToInput rawTest

res1 :: Result1
res1 = 566
res2 :: Result2
res2 = 891684
