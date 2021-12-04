module AoC01
  ( solve01 )
  where


-- Part 1
f1 :: [Int] -> Int
f1 l = sum $ map fromEnum $ zipWith (<) l (tail l)

-- Part 2
f2 :: [Int] -> Int
f2 l = f1 $ map (\(a, b, c) -> a + b + c) $ zip3 l (tail l) (tail $ tail l)

-- Main
linesToInput :: [String] -> [Int]
linesToInput = map read

solve01 :: FilePath -> IO (Int, Int)
solve01 fp = do
  raw <- readFile fp
  let input = linesToInput $ lines raw
  return (f1 input, f2 input)



-- Tests
testLines =
  [ "199"
  , "200"
  , "208"
  , "210"
  , "200"
  , "207"
  , "240"
  , "269"
  , "260"
  , "263"
  ]

test = linesToInput testLines

file = "../data/input-01.txt"

res1 = 1832
res2 = 1858
