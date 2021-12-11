module AoC09
  ( solve09 )
  where

import Data.List (intercalate) -- for test
import Data.List (findIndices, transpose, groupBy, nub, intersect, sortBy)


-- Part 1
lineLows :: [Int] -> [Int]
lineLows l = findIndices id
             $ zipWith3 (\p c n -> p > c && c < n) l' (tail l') (tail $ tail l')
  where l' = 9 : l ++ [9]

f1 :: [[Int]] -> Int
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

f2 :: [[Int]] -> Int
f2 = product . take 3 . sortBy (flip compare)  . map size . toTree [] . non9

-- Main
rawToInput :: String -> [[Int]]
rawToInput = map (map (read . flip (:) [])) . lines

solve09 :: String -> (Int, Int)
solve09 raw = (f1 input, f2 input)
  where input = rawToInput raw



-- Tests
run :: IO (Int, Int)
run = do
  raw <- readFile "data/AoCInput9"
  return $ solve09 raw

test :: (Int, Int)
test = solve09 rawTest

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

inputTest :: [[Int]]
inputTest = rawToInput rawTest

res1 :: Int
res1 = 566
res2 :: Int
res2 = 891684
