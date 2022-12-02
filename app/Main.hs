module Main where

import D01 ( d01_1, d01_2 )

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Solution = [String] -> String

solutions :: IntMap [Solution]
solutions = M.fromList $ zip [1 ..] [
    [d01_1, d01_2]
  ]

runDay :: String -> IO ()
runDay d = do
  let p     = concat ["./input/d", if length d < 2 then '0' : d else d, ".txt"]
      mSols = sequenceA $ readMaybe d >>= flip M.lookup solutions
  b      <- doesFileExist p
  mInput <- if b then Just . lines <$> readFile p else pure Nothing
  case traverse (<*> mInput) mSols of
    Just os -> mapM_ putStrLn $ zipWith (\i s -> concat ["Part ", show i, ": ", s]) [1 :: Int ..] os
    Nothing -> putStrLn "Challenge not found!"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [d] -> runDay d
    _ -> putStrLn "Invalid number of arguments"
