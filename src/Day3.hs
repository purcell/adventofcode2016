module Day3
  ( day3
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String
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

parseTriangle :: Parser Triangle
parseTriangle = Triangle <$> spaceNumber <*> spaceNumber <*> spaceNumber

loadInputByRows :: IO [Triangle]
loadInputByRows = do
  result <-
    parseFromFile (many1 (parseTriangle <* newline) <* eof) "input/3.txt"
  case result of
    Right triangles -> return triangles
    Left e -> error (show e)

groupsOf :: Int -> [a] -> [[a]]
groupsOf = go []
  where
    go acc _ [] = acc
    go acc n xs = go (before : acc) n after
      where
        (before, after) = splitAt n xs

loadInputByCols :: IO [Triangle]
loadInputByCols = do
  result <-
    parseFromFile (many1 (count 3 spaceNumber <* newline) <* eof) "input/3.txt"
  case result of
    Right rows -> return $ toTriangle <$> groupsOf 3 (concat (transpose rows))
      where toTriangle [a, b, c] = Triangle a b c
    Left e -> error (show e)

day3 :: IO ()
day3 = do
  rowTriangles <- loadInputByRows
  print $ length (filter possible rowTriangles)
  colTriangles <- loadInputByCols
  print $ length (filter possible colTriangles)
