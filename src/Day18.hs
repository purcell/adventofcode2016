module Main where

import Day
import Data.List (tails)

data Tile
  = Safe
  | Trap
  deriving (Eq)

next :: [Tile] -> [Tile]
next tiles =
  fmap tripleToNext .
  take (length tiles) . fmap (take 3) . tails . cycle . (Safe :) $
  tiles
  where
    tripleToNext :: [Tile] -> Tile
    tripleToNext [Trap, Trap, Safe] = Trap
    tripleToNext [Safe, Trap, Trap] = Trap
    tripleToNext [Trap, Safe, Safe] = Trap
    tripleToNext [Safe, Safe, Trap] = Trap
    tripleToNext _ = Safe

safeInRows n = length . filter (== Safe) . concat . take n . iterate next

partA = safeInRows 40

partB = safeInRows 400000

parseTile = (char '.' *> return Safe) <|> (char '^' *> return Trap)

main =
  runDay $
  Day
    18
    (many1 parseTile <* newline)
    (return . show . partA)
    (return . show . partB)
