module Main where

import AdventOfCode
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

main =
  runDay $
  Day
    6
    (many1 (many1 letter <* newline))
    (return . decodeMostFrequent)
    (return . decodeLeastFrequent)
