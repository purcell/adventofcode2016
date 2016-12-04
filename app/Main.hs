module Main where

import           Lib

banner n = putStrLn $ "==== DAY " ++ show n

main :: IO ()
main = do
  banner 1
  day1
  banner 2
  day2
  banner 3
  day3
  banner 4
  day4
