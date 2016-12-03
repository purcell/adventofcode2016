module Main where

import           Lib

banner n = putStrLn $ "==== DAY " ++ show n

main :: IO ()
main = do
  banner 1
  day1
  banner 2
  day2
