module Day13
  ( day13
  ) where

import Day
import Data.Bits (popCount)
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Set (Set)
import qualified Data.Set as S

type Coord = (Int, Int)

puzzleInput = 1350

isOpen :: Coord -> Bool
isOpen (x, y) = even $ bitsSet $ x * x + 3 * x + 2 * x * y + y + y * y + puzzleInput
  where
    bitsSet = popCount

positionTree :: Coord -> Tree [Coord]
positionTree startPos = T.unfoldTree unfolder (S.singleton startPos, [startPos])
  where
    unfolder :: (Set Coord, [Coord]) -> ([Coord], [(Set Coord, [Coord])])
    unfolder (seen, hist@(cur:_)) =
      ( hist
      , [ (S.fromList nexts `S.union` seen, next : hist)
        | next <- nexts ])
      where
        nexts =
          [ next
          | next <- nextPositions cur
          , not (next `S.member` seen) ]
    unfolder _ = undefined
    nextPositions (x, y) =
      [ newpos
      | newpos@(x', y') <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      , x' >= 0 && y' >= 0
      , isOpen newpos ]

showMap :: Int -> Int -> String
showMap w h = unlines $ showRow <$> [0 .. h]
  where
    showRow y =
      [ if isOpen (x, y)
         then '.'
         else '#'
      | x <- [0 .. w] ]

partA = length path - 1
  where
    path =
      head $
      dropWhile ((/= (31, 39)) . head) $ concat $ T.levels $ positionTree (1, 1)

day13 =
  Day
    13
    (many anyChar)
    (return . show . const partA)
    (return . show . const "TODO")
