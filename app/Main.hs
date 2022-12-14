
module Main where

import D01 ( d01 )
import D02 ( d02 )
import D03 ( d03 )
import D04 ( d04 )
import D05 ( d05 )
import D06 ( d06 )
import D07 ( d07 )
import D08 ( d08 )
import D09 ( d09 )
import D10 ( d10 )
import D11 ( d11 )
import D12 ( d12 )
import D13 ( d13 )
import D14 ( d14 )

import System.Directory (doesFileExist)
import qualified Data.IntMap.Strict as M

type Solution = FilePath -> Text -> (String, String)

solutions :: IntMap Solution
solutions = M.fromList $ zip [1 ..]
  [ d01
  , d02
  , d03
  , d04
  , d05
  , d06
  , d07
  , d08
  , d09
  , d10
  , d11
  , d12
  , d13
  , d14
  ]

runDay :: String -> IO ()
runDay d = do
  let d'   = if length d < 2 then '0' : d else d
      p    = concat ["./input/d", d', ".txt"]
      mSol = readMaybe d >>= flip M.lookup solutions
  b      <- doesFileExist p
  mInput <- if b then Just . decodeUtf8 <$> readFileBS p else pure Nothing
  putStrLn $ concat ["*** Day ", d', " ***\n"]
  case mSol <*> pure p <*> mInput of
    Just as -> bitraverse_ (putStrLn . ("Part 1: " ++)) (putStrLn . ("Part 2: " ++)) as
    Nothing -> putStrLn "Challenge not found!"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [d] -> runDay d
    _ -> putStrLn "Invalid number of arguments"
