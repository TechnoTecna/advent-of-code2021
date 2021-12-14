module Main where

import qualified AoC01
import qualified AoC02
import qualified AoC03
import qualified AoC04
import qualified AoC05
import qualified AoC06
import qualified AoC07
import qualified AoC08
import qualified AoC09
import qualified AoC10
import qualified AoC11
import qualified AoC12
import qualified AoC13
import Control.Monad (zipWithM)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))
import Text.Read (readMaybe)


data Request = Dir FilePath Request | All | Some [Int] | File Int FilePath
  deriving (Show)
data Result = SomeR  [(Int, (String, String))]
            | FileR Int FilePath (String, String)

defFolder :: FilePath
defFolder = "data"

solvers :: [String -> (String, String)]
solvers =
  [ AoC01.solve
  , AoC02.solve
  , AoC03.solve
  , AoC04.solve
  , AoC05.solve
  , AoC06.solve
  , AoC07.solve
  , AoC08.solve
  , AoC09.solve
  , AoC10.solve
  , AoC11.solve
  , AoC12.solve
  , AoC13.solve
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

solve :: Int -> String -> (String, String)
solve i = solvers !! i

solveM :: [Int] -> [String] -> [(String, String)]
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

showRes :: (String, String) -> String
showRes (p1, p2) =
  "  Part 1: " ++ head p1s ++ "\n"
               ++ intercalate "\n" (map ("          " ++) (tail p1s)) ++
  "  Part 2: " ++ head p2s ++ "\n"
               ++ intercalate "\n" (map ("          " ++) (tail p2s))
  where p1s = lines p1
        p2s = lines p2

printRes :: Result -> String
printRes (SomeR l) =
  concatMap (\(a, b) -> "day " ++ show a ++ ":" ++ (if a<10 then "  " else " ")
                        ++ "\n" ++ showRes b)
            l
printRes (FileR i fp r) = "day " ++ show i ++ " with " ++ show fp ++ ": \n"
                          ++ showRes r

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
