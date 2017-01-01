module Main where

import AdventOfCode
import Data.List (delete)
import Data.Maybe (mapMaybe, fromJust, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Function (on)

data Pos = Pos
  { px :: !Int
  , py :: !Int
  } deriving (Eq, Show)

instance Ord Pos where
  compare = compare `on` (py &&& px)

data Node = Node
  { nPos :: !Pos
  , nSize :: !Int
  , nUsed :: !Int
  } deriving (Eq, Ord, Show)

parseNode :: Parser Node
parseNode =
  Node <$>
  (Pos <$> (string "/dev/grid/node-x" *> number) <*> (string "-y" *> number)) <*>
  amountCol <*>
  (amountCol <* manyTill anyChar newline)
  where
    number = read <$> many1 digit
    amountCol = many1 space *> number <* char 'T'

viablePairs :: [Node] -> [(Node, Node)]
viablePairs nodes =
  [ (n1, n2)
  | n1 <- nodes
  , n2 <- delete n1 nodes
  , viable n1 n2 ]

viable :: Node -> Node -> Bool
viable n1 n2 = n1 /= n2 && nUsed n1 > 0 && nUsed n1 <= (nSize n2 - nUsed n2)

data Grid = Grid
  { gridNodes :: !(Map Pos Node)
  , gridBottomRight :: !Pos
  , gridGoalData :: !Pos
  } deriving (Eq, Ord)

makeGrid :: [Node] -> Grid
makeGrid nodes = Grid (M.fromList ((nPos &&& id) <$> nodes)) bottomRight topRight
  where
    bottomRight = maximum (nPos <$> nodes)
    topRight = Pos (px bottomRight) 0

findNode :: Grid -> Pos -> Maybe Node
findNode g p = p `M.lookup` gridNodes g

neighbours :: Grid -> Node -> [Node]
neighbours g (Node (Pos x y) _ _) =
  mapMaybe
    (findNode g . uncurry Pos)
    [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

origin :: Pos
origin = Pos 0 0

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (Pos x1 y1) (Pos x2 y2) = abs (x2 - x1) + abs (y2 - y1)

partA :: [Node] -> Int
partA = length . viablePairs

partB :: [Node] -> (State, Int)
partB nodes = head $ filter (reachedOrigin . fst) $ astar (nexts grid) initialState
  where
    reachedOrigin = (origin ==) . sGoalData
    initialState = State (fromJust (findEmpty grid)) (gridGoalData grid)
    grid = makeGrid nodes

findEmpty :: Grid -> Maybe Pos
findEmpty = listToMaybe . fmap nPos . filter ((== 0) . nUsed) . M.elems . gridNodes

data State = State
  { sEmpty :: !Pos
  , sGoalData :: !Pos
  } deriving (Eq, Ord, Show)

nexts :: Grid -> State -> [(State, Int, Int)]
nexts g (State emptypos goalpos) =
  let empty = fromJust (findNode g emptypos)
  in do neighbour <-
          [ n
          | n <- neighbours g empty
          , nUsed n <= nSize empty ]
        let goalpos' =
              if goalpos == nPos neighbour
                then emptypos
                else goalpos
            empty' = nPos neighbour
        return
          ( State empty' goalpos'
          , 1
          , manhattanDistance goalpos' origin + manhattanDistance empty' goalpos' -
            1)

main :: IO ()
main = runDay day22

day22 :: Day [Node]
day22 =
  Day
    22
    (count 2 (manyTill anyChar newline) *> many1 parseNode)
    (return . show . partA)
    (return . show . partB)
