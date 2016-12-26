module Main where

import Day
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import Data.Monoid ((<>))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.List (sort, permutations, minimumBy)
import Control.Arrow ((&&&))

data Pos = Pos
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

data Grid = Grid
  { gSquares :: Map Pos Square
  , gGoals :: [(Int, Pos)]
  }

data Square
  = Wall
  | Open
  | Goal Int
  deriving (Eq)

goalPos :: Grid -> Int -> Maybe Pos
goalPos g n = lookup n (gGoals g)

goalPositions :: Grid -> [Pos]
goalPositions = map snd . gGoals

type Path = Seq Pos

squareAt :: Grid -> Pos -> Maybe Square
squareAt g p = p `M.lookup` gSquares g

shortestPathFrom :: Grid -> Pos -> Pos -> Maybe Path
shortestPathFrom g start dest =
  go (S.singleton start) ((Sq.singleton . Sq.singleton) start)
  where
    go _ s
      | Sq.null s = Nothing
    go seen ps =
      let path = Sq.index ps 0
          curpos = Sq.index path 0
          rest = Sq.drop 1 ps
          nexts = filter (not . (`S.member` seen)) (openNeighbours g curpos)
          newPaths = (Sq.<| path) <$> Sq.fromList nexts
      in if dest == curpos
           then Just path
           else go (seen `S.union` S.fromList nexts) (rest <> newPaths)

openNeighbours :: Grid -> Pos -> [Pos]
openNeighbours g (Pos x y) = do
  p' <- [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]
  let Just s = squareAt g p'
  guard (s /= Wall)
  return p'

makeGrid :: [[Square]] -> Grid
makeGrid sqs = Grid squareMap goals
  where
    goals =
      sort
        [ (n, p)
        | (p, Goal n) <- M.toList squareMap ]
    squareMap =
      M.fromList
        [ (Pos x y, sq)
        | (y, row) <- zip [0 ..] sqs
        , (x, sq) <- zip [0 ..] row ]

parseGrid :: Parser Grid
parseGrid = makeGrid <$> many1 (many1 square <* newline)
  where
    square =
      (char '#' *> pure Wall) <|> (char '.' *> pure Open) <|>
      (Goal . read <$> count 1 digit)

main :: IO ()
main = runDay day24

shortestVariation :: Grid
                  -> ([(Int, Pos)] -> [[(Int, Pos)]])
                  -> ([(Int, Pos)], Int)
shortestVariation g goalVariations =
  minimumBy (compare `on` snd) ((id &&& distance) <$> goalVariations goals)
  where
    distance gs = sum ((fromJust . (`lookup` distances)) <$> zip gs (drop 1 gs))
    goals = gGoals g
    distances =
      [ ((start, dest), length path - 1)
      | start <- goals
      , dest <- goals
      , start /= dest
      , let Just path = shortestPathFrom g (snd start) (snd dest) ]

partA :: Grid -> ([(Int, Pos)], Int)
partA g = shortestVariation g makePaths
  where
    makePaths goals = (take 1 goals ++) <$> permutations (drop 1 goals)

partB :: Grid -> ([(Int, Pos)], Int)
partB g = shortestVariation g makePaths
  where
    makePaths goals =
      (\middle -> first <> middle <> first) <$> permutations (drop 1 goals)
      where
        first = take 1 goals

day24 :: Day Grid
day24 = Day 24 parseGrid (return . show . partA) (return . show . partB)
