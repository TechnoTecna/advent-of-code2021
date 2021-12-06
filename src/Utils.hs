module Utils
  ( splitWhen, first2, breakOnList, splitOnList, vecDiff, vecSum, dumbNorm
  , count )
  where

import Data.Bifunctor (first)
import Data.List (isPrefixOf)


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
                  [] -> []
                  t  -> w : splitWhen p t2
                    where (w, t2) = break p t

breakOnList :: Eq a => [a] -> [a] -> ([a], [a])
breakOnList _ []       = ([], [])
breakOnList brk l@(h:t)
  | brk `isPrefixOf` l = ([], l)
  | otherwise          = first (h :) $ breakOnList brk t

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList _   []        = []
splitOnList brk s@(h:t)
  | brk `isPrefixOf` s = splitOnList brk $ drop (length brk) s
  | otherwise          = w : splitOnList brk t2
                       where (w, t2) = breakOnList brk s

first2 :: [a] -> (a, a)
first2 (a:b:_) = (a, b)

vecDiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecDiff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vecSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dumbNorm :: Int -> Int
dumbNorm i = case compare i 0 of
                 LT -> -1
                 EQ -> 0
                 GT -> 1

count :: (a -> Bool) -> [a] -> Int
count p = sum . map (fromEnum . p)
