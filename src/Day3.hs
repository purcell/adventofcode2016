module Day3
  ( day3
  )
where

import           Text.Parsec        hiding (State)
import           Text.Parsec.String



data Triangle = Triangle Int Int Int

possible :: Triangle -> Bool
possible (Triangle a b c) = all valid $ zip3 sides (drop 1 $ cycle sides) (drop 2 $ cycle sides)
  where
    sides = [a, b, c]
    valid (s1, s2, s3) = s1 + s2 > s3


parseTriangle :: Parser Triangle
parseTriangle = Triangle <$> spaceNumber <*> spaceNumber <*> spaceNumber
  where
    spaceNumber = spaces *> parseInt
    parseInt = read <$> many1 digit

loadInput :: IO [Triangle]
loadInput = do
  result <- parseFromFile (many1 (parseTriangle <* newline) <* eof) "input/3.txt"
  case result of
    Right triangles -> return triangles
    Left e -> error (show e)

day3 :: IO ()
day3 = do
  triangles <- loadInput
  print $ length (filter possible triangles)
