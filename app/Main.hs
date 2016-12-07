module Main where

import Control.Monad (forM_)
import Lib
import System.Environment (getArgs)

day :: String -> IO ()
day n =
  runDay $
  case n of
    "1" -> day1
    "2" -> day2
    "3" -> day3
    "4" -> day4
    "5" -> day5
    "6" -> day6
    "7" -> day7
    _ -> error $ "No such day " ++ show n
  where
    runDay action = banner >> action
    banner = putStrLn $ "==== DAY " ++ n

main :: IO ()
main = do
  args <- getArgs
  forM_ args day
