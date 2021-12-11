module Main where

import AoC01 (solve01)
import AoC02 (solve02)
import AoC03 (solve03)
import AoC04 (solve04)
import AoC05 (solve05)
import AoC06 (solve06)
import AoC07 (solve07)
import AoC08 (solve08)
import AoC09 (solve09)
import AoC10 (solve10)
import AoC11 (solve11)
import Control.Monad (zipWithM)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))
import Text.Read (readMaybe)


data Request = Dir FilePath Request | All | Some [Int] | File Int FilePath
  deriving (Show)
data Result = SomeR  [(Int, (Int, Int))]
            | FileR Int FilePath (Int, Int)

defFolder :: FilePath
defFolder = "data"

solvers :: [String -> (Int, Int)]
solvers =
  [ solve01
  , solve02
  , solve03
  , solve04
  , solve05
  , solve06
  , solve07
  , solve08
  , solve09
  , solve10
  , solve11
  ]

parseReq :: [String] -> Maybe Request
parseReq [] = Just All
parseReq ["-f", fp, i] =
  case readMaybe i of
    Nothing -> Nothing
    Just i' -> Just $ File i' fp
parseReq ["-d", fp] = Just $ Dir fp All
parseReq ("-d":fp:is) =
  case mapM readMaybe is of
    Nothing  -> Nothing
    Just is' -> Just $ Dir fp (Some is')
parseReq is =
  case mapM readMaybe is of
    Nothing -> Nothing
    Just is' -> Just $ Some is'

solve :: Int -> String -> (Int, Int)
solve i = solvers !! i

solveM :: [Int] -> [String] -> [(Int, Int)]
solveM = zipWith ((!!) solvers . (-1 +))

getInputs :: FilePath -> [Int] -> IO [String]
getInputs fp = mapM (readFile . (</>) fp . (++) "AoCInput" . show)

process :: FilePath -> Request -> IO Result
process _ (Dir fp r) = process fp r
process fp All = do
  let labels = [1..length solvers]
  inputs <- getInputs fp labels
  let sols = solveM labels inputs
  let labeled = zip labels sols
  return $ SomeR labeled
process fp (Some is) = do
  if any (\x -> x > length solvers || 1 > x) is then do
    putStrLn "ERROR: Some requested days are not implemented"
    exitFailure
  else do
    inputs <- getInputs fp is
    let sols = solveM is inputs
    let labeled = zip is sols
    return $ SomeR labeled
process _ (File i fp) = do
  input <- readFile fp
  let sol = solve (i-1) input
  return $ FileR i fp sol

printRes :: Result -> String
printRes (SomeR l) =
  intercalate "\n"
  $ map (\(a, b) -> "day " ++ show a ++ ":" ++ (if a<10 then "  " else " ")
                    ++ show b)
        l
printRes (FileR i fp r) = "day " ++ show i ++ " with " ++ show fp ++ ": "
                          ++ show r

main :: IO ()
main = do
  args <- getArgs
  let req = parseReq args
  case req of
    Nothing -> do
      putStrLn "ERROR: Couldn't parse arguments"
      exitFailure
    Just req' -> do
      res <- process defFolder req'
      putStrLn $ printRes res
      exitSuccess
