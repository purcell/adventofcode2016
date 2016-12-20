{-# LANGUAGE TupleSections #-}

module Main where

import Day
import Data.List (sort)

type IPRange = (Integer, Integer)

parseIP :: Parser IPRange
parseIP = (,) <$> parseInt <*> (char '-' *> parseInt)
  where
    parseInt = read <$> many1 digit

lowest = go 1 . sort
  where
    go n [] = n
    go n ((a, _):_)
      | n < a = n
    go n ((a, b):(a', b'):xs)
      | b > a' = go n ((a, b') : xs)
    go _ ((a, b):(a', b'):xs) = go (b + 1) ((a', b') : xs)
    go _ [(_, b)] = b + 1

partA = lowest

main =
  runDay $
  Day
    20
    (many1 (parseIP <* newline))
    (return . show . partA)
    (return . show . const "TODO")
