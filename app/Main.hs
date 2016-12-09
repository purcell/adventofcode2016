module Main where

import Control.Monad (forM_)
import Lib
import System.Environment (getArgs)

day :: String -> IO ()
day n =
  case n of
    "1" -> runDay day1
    "2" -> runDay day2
    "3" -> runDay day3
    "4" -> runDay day4
    "5" -> runDay day5
    "6" -> runDay day6
    "7" -> runDay day7
    "8" -> runDay day8
    "9" -> runDay day9
    _ -> error $ "No such day " ++ show n

main :: IO ()
main = do
  args <- getArgs
  forM_ args day
