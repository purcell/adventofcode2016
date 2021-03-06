module Main where

import AdventOfCode
import Data.Bits (popCount)
import Data.List (nub)
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

nextPaths :: [Coord] -> [[Coord]]
nextPaths p@(cur:_) = (: p) <$> nextPositions cur
nextPaths _ = undefined

nextPositions :: Coord -> [Coord]
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

paths = bfsOn head nextPaths [(1, 1)]

partA = length (head $ dropWhile ((/= (31, 39)) . head) paths) - 1

partB = length $ nub $ concat $ takeWhile ((<= 51) . length) paths

main =
  runDay $
  Day
    13
    (many anyChar)
    (return . show . const partA)
    (return . show . const partB)
