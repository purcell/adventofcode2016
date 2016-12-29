module Main where

import AdventOfCode
import Data.Set (Set)
import qualified Data.Set as S

type PixelSet = Set (Int, Int)

data Screen = Screen
  { sMaxX :: Int
  , sMaxY :: Int
  , sPix :: PixelSet
  }

instance Show Screen where
  show s = unlines $ map showRow [0 .. (sMaxY s - 1)]
    where
      showRow :: Int -> String
      showRow y =
        [ if (x, y) `S.member` sPix s
           then '#'
           else '.'
        | x <- [0 .. (sMaxX s - 1)] ]

initialScreen :: Screen
initialScreen = Screen 50 6 S.empty

modScreen :: Screen -> (PixelSet -> PixelSet) -> Screen
modScreen s f =
  s
  { sPix = f (sPix s)
  }

applyInstruction :: Screen -> Instruction -> Screen
applyInstruction s (Rect w h) = modScreen s (S.union newPix)
  where
    newPix =
      S.fromList
        [ (x, y)
        | x <- [0 .. (w - 1)]
        , y <- [0 .. (h - 1)] ]
applyInstruction s (Rotate Col idx off) = modScreen s (S.map rotateCol)
  where
    rotateCol (x, y) =
      if x == idx
        then (x, (y + off) `mod` sMaxY s)
        else (x, y)
applyInstruction s (Rotate Row idx off) = modScreen s (S.map rotateRow)
  where
    rotateRow (x, y) =
      if y == idx
        then ((x + off) `mod` sMaxX s, y)
        else (x, y)

litPixels :: Screen -> Int
litPixels = length . S.toList . sPix

data Axis
  = Col
  | Row

data Instruction
  = Rect { rectW :: Int
         , rectH :: Int}
  | Rotate { rotAxis :: Axis
           , rotIndex :: Int
           , rotOffset :: Int}

parseInstruction :: Parser Instruction
parseInstruction =
  try (Rect <$> (string "rect " *> number) <*> (string "x" *> number)) <|>
  (string "rotate " *>
   ((Rotate Col <$> (string "column x=" *> number) <*> (string " by " *> number)) <|>
    (Rotate Row <$> (string "row y=" *> number) <*> (string " by " *> number))))
  where
    number = read <$> many1 digit

run :: [Instruction] -> Screen
run = foldl applyInstruction initialScreen

main =
  runDay $
  Day
    8
    (many1 (parseInstruction <* newline))
    (return . show . litPixels . run)
    (return . show . run)
