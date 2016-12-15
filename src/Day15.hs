module Day15
  ( day15
  ) where

import Day

data Disc = Disc
  { dSize :: Int
  , dPos :: Int
  }

partA discs =
  take
    1
    [ t
    | t <- [0 ..]
    , all
       (== 0)
       [ slotOffsetAt (t + t') d
       | (d, t') <- zip discs [1 ..] ] ]

partB discs = partA (discs ++ [Disc 11 0])

slotOffsetAt t (Disc size pos) = (pos + t) `mod` size

exampleDiscs = [Disc 5 4, Disc 2 1]

parseDisc :: Parser Disc
parseDisc =
  Disc <$>
  (string "Disc #" >> number >> string " has " *> number <* string " positions") <*>
  (string "; at time=0, it is at position " *> number <* string ".")
  where
    number = read <$> many1 digit

day15 =
  Day
    15
    (many1 (parseDisc <* newline))
    (return . show . partA)
    (return . show . partB)
