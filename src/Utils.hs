{-# LANGUAGE GADTs #-}

module Utils
  ( splitWhen, first2, breakOnList, splitOnList, vecDiff, vecSum, dumbNorm
  , count , printL, Map (..), setWith, set, get)
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

printL :: Show a => [a] -> IO ()
printL l = do
  let strLn = map show l
  let res = foldr1 ((++) . flip (++) "\n") strLn
  putStrLn res

data Map k v where
  Branch :: Ord k => k -> v -> Map k v -> Map k v -> Map k v
  Leaf   :: Ord k =>                                 Map k v

show' :: (Show k, Show v) => String -> Map k v -> String
show' i Leaf = i ++ "Lef"
show' i (Branch k v l r) = i ++ "Brc key:" ++ show k ++ " val:" ++ show v
                           ++ "\n" ++ show' (i ++ "  ") l
                           ++ "\n" ++ show' (i ++ "  ") r

instance (Show k, Show v) => Show (Map k v) where
  show = show' ""

setWith :: Ord k => Map k v -> (v -> v) -> k -> v -> Map k v
setWith Leaf f k v = Branch k v Leaf Leaf
setWith (Branch k' v' l r) f k v = case k `compare` k' of
                                     LT -> Branch k' v' (setWith l f k v) r
                                     GT -> Branch k' v' l (setWith r f k v)
                                     EQ -> Branch k (f v') l r

set :: Ord k => Map k v -> k -> v -> Map k v
set m k v = setWith m (const v) k v

get :: Ord k => Map k v -> k -> Maybe v
get Leaf _ = Nothing
get (Branch k' v l r) k = case k `compare` k' of
                            LT -> get l k
                            GT -> get r k
                            EQ -> Just v
