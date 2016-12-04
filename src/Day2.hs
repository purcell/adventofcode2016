module Day2
  ( day2
  )
where

import Data.Maybe (fromMaybe)
import           Text.Parsec        hiding (State)
import           Text.Parsec.String
import Data.Map (Map)
import qualified Data.Map as M


data Direction = U | D | L | R

data Button = Button { bNum :: String, bPos :: (Int, Int) }
  deriving Show

type Grid = (Int, Int) -> Maybe String

makeGrid :: [String] -> Grid
makeGrid gridRows = flip M.lookup gridMap
  where
    gridMap = M.fromList [((x, y), [c]) | (y, l) <- lines, (x, c) <- line l, c /= ' ']
    lines = zip [0..] gridRows
    line = zip [0..]

squareGrid = makeGrid ["123" ,"456" ,"798"]
diamondGrid = makeGrid [ "  1  "
                       , " 234 "
                       , "56789"
                       , " ABC "
                       , "  D  "]

maybeButton :: Grid -> (Int, Int) -> Maybe Button
maybeButton grid pos = (\n -> Button n pos) <$> grid pos

neighbour :: Grid -> Button -> Direction -> Maybe Button
neighbour grid (Button _ (x, y)) U = maybeButton grid (x, y - 1)
neighbour grid (Button _ (x, y)) D = maybeButton grid (x, y + 1)
neighbour grid (Button _ (x, y)) L = maybeButton grid (x - 1, y)
neighbour grid (Button _ (x, y)) R = maybeButton grid (x + 1, y)

move :: Grid -> Button -> Direction -> Button
move grid b d = fromMaybe b $ neighbour grid b d

code :: Grid -> Button -> [[Direction]] -> [String]
code grid init lines = bNum <$> tail (scanl followLine init lines)
  where
    followLine = foldl (move grid)

parseDirection :: Parser Direction
parseDirection = string "U" *> return U
                 <|> string "D" *> return D
                 <|> string "L" *> return L
                 <|> string "R" *> return R

loadInput :: IO [[Direction]]
loadInput = do
  result <- parseFromFile (many1 (many1 parseDirection <* newline) <* eof) "input/2.txt"
  case result of
    Right directions -> return directions
    Left e -> error (show e)

day2 :: IO ()
day2 = do
  directions <- loadInput
  print $ code squareGrid (Button "5" (1,1)) directions
  print $ code diamondGrid (Button "5" (0, 2)) directions
