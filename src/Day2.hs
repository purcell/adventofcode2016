module Day2
  ( day2
  ) where

import Day
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Direction
  = U
  | D
  | L
  | R

data Button = Button
  { bNum :: String
  , bPos :: (Int, Int)
  } deriving (Show)

type Grid = (Int, Int) -> Maybe String

makeGrid :: [String] -> Grid
makeGrid gridRows = flip M.lookup gridMap
  where
    gridMap =
      M.fromList
        [ ((x, y), [c])
        | (y, l) <- lines
        , (x, c) <- line l
        , c /= ' ' ]
    lines = zip [0 ..] gridRows
    line = zip [0 ..]

squareGrid = makeGrid ["123", "456", "798"]

diamondGrid = makeGrid ["  1  ", " 234 ", "56789", " ABC ", "  D  "]

maybeButton :: Grid -> (Int, Int) -> Maybe Button
maybeButton grid pos = flip Button pos <$> grid pos

neighbour :: Grid -> Button -> Direction -> Maybe Button
neighbour grid (Button _ (x, y)) U = maybeButton grid (x, y - 1)
neighbour grid (Button _ (x, y)) D = maybeButton grid (x, y + 1)
neighbour grid (Button _ (x, y)) L = maybeButton grid (x - 1, y)
neighbour grid (Button _ (x, y)) R = maybeButton grid (x + 1, y)

move :: Grid -> Button -> Direction -> Button
move grid b d = fromMaybe b $ neighbour grid b d

code :: Grid -> Button -> [[Direction]] -> String
code grid init lines = concatMap bNum $ tail (scanl followLine init lines)
  where
    followLine = foldl (move grid)

parseDirection :: Parser Direction
parseDirection =
  string "U" *> return U <|> string "D" *> return D <|> string "L" *> return L <|>
  string "R" *> return R

day2 =
  Day
    2
    (many1 (many1 parseDirection <* newline))
    (return . code squareGrid (Button "5" (1, 1)))
    (return . code diamondGrid (Button "5" (0, 2)))
