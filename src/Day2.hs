module Day2
  ( day2
  )
where

import Data.Maybe (fromMaybe)
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data Direction = U | D | L | R

keypadWidth = 3

data Button = Button { bNum :: Int }
  deriving Show

sameRow :: Int -> Int -> Bool
sameRow a b = (a - 1) `div` keypadWidth == (b - 1) `div` keypadWidth

neighbour :: Button -> Direction -> Maybe Button
neighbour (Button n) U = if n - keypadWidth >= 1
                         then Just $ Button (n - keypadWidth)
                         else Nothing
neighbour (Button n) D = if n + keypadWidth <= keypadWidth * keypadWidth
                         then Just $ Button (n + keypadWidth)
                         else Nothing
neighbour (Button n) L = if sameRow n (n - 1)
                         then Just $ Button (n - 1)
                         else Nothing
neighbour (Button n) R = if sameRow n (n + 1)
                         then Just $ Button (n + 1)
                         else Nothing

move :: Button -> Direction -> Button
move b d = fromMaybe b $ neighbour b d



code :: [[Direction]] -> [Int]
code lines = bNum <$> tail (scanl followLine (Button 5) lines)
  where
    followLine = foldl move

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
  print $ code directions
