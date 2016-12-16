module Main where

import Day
import Data.List (transpose, splitAt)

data Triangle =
  Triangle Int
           Int
           Int

possible :: Triangle -> Bool
possible (Triangle a b c) =
  all valid $ zip3 sides (drop 1 $ cycle sides) (drop 2 $ cycle sides)
  where
    sides = [a, b, c]
    valid (s1, s2, s3) = s1 + s2 > s3

spaceNumber :: Parser Int
spaceNumber = spaces *> parseInt
  where
    parseInt = read <$> many1 digit

groupsOf :: Int -> [a] -> [[a]]
groupsOf = go []
  where
    go acc _ [] = acc
    go acc n xs = go (before : acc) n after
      where
        (before, after) = splitAt n xs

toTriangle [a, b, c] = Triangle a b c

main =
  runDay $
  Day
    3
    (many1 (count 3 spaceNumber <* newline))
    (return . show . length . filter possible . fmap toTriangle)
    (return .
     show . length . filter possible . fmap toTriangle . groupsOf 3 . concat . transpose)
