{-# LANGUAGE TupleSections #-}

module Main where

import AdventOfCode
import Data.List (sort)

type IPRange = (Integer, Integer)

rangeLen (a, b) = 1 + (b - a)

parseIP :: Parser IPRange
parseIP = (,) <$> parseInt <*> (char '-' *> parseInt)
  where
    parseInt = read <$> many1 digit

lowest :: [IPRange] -> Integer
lowest = go 1 . combineRanges
  where
    go n [] = n
    go n ((a, _):_)
      | n < a = n
    go _ ((_, b):xs) = go (b + 1) xs

combineRanges :: [IPRange] -> [IPRange]
combineRanges = combine . sort
  where
    combine (r1@(_, b):r2@(a', _):xs)
      | a' > b = r1 : combine (r2 : xs)
    combine ((a, b):(_, b'):xs) = combine ((a, max b b') : xs)
    combine x = x

partA = lowest

partB = (4294967296 -) . sum . map rangeLen . combineRanges

main =
  runDay $
  Day
    20
    (many1 (parseIP <* newline))
    (return . show . partA)
    (return . show . partB)
