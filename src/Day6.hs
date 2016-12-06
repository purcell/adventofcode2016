module Day6
  ( day6
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.List (sort, group, transpose)
import Control.Arrow ((&&&))

decode :: ([(Int, Char)] -> (Int, Char)) -> [String] -> String
decode chooser = map ((snd . chooser) . column) . transpose
  where
    column = map (length &&& head) . group . sort

decodeMostFrequent :: [String] -> String
decodeMostFrequent = decode maximum

decodeLeastFrequent :: [String] -> String
decodeLeastFrequent = decode minimum

loadInput :: IO [String]
loadInput = do
  result <- parseFromFile (many1 (many1 letter <* newline) <* eof) "input/6.txt"
  case result of
    Right xs -> return xs
    Left e -> error (show e)

day6 :: IO ()
day6 = do
  strings <- loadInput
  putStrLn $ decodeMostFrequent strings
  putStrLn $ decodeLeastFrequent strings
