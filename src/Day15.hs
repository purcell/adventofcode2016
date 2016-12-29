module Main where

import AdventOfCode

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
       [ pos + (t + t') `mod` size
       | (Disc size pos, t') <- zip discs [1 ..] ] ]

partB = partA . (++ [Disc 11 0])

exampleDiscs = [Disc 5 4, Disc 2 1]

parseDisc :: Parser Disc
parseDisc =
  Disc <$>
  (string "Disc #" >> number >> string " has " *> number <* string " positions") <*>
  (string "; at time=0, it is at position " *> number <* string ".")
  where
    number = read <$> many1 digit

main =
  runDay $
  Day
    15
    (many1 (parseDisc <* newline))
    (return . show . partA)
    (return . show . partB)
