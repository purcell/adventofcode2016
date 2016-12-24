module Main where

import Day
import Debug.Trace
import Data.List (delete, groupBy, sortBy, find)
import qualified Data.Set as S
import Data.Maybe (mapMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Function (on)

data Pos = Pos
  { px :: !Int
  , py :: !Int
  } deriving (Eq)

instance Ord Pos where
  compare = compare `on` (py &&& px)

data Node = Node
  { nPos :: !Pos
  , nSize :: !Int
  , nUsed :: !Int
  } deriving (Eq, Ord)

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

goalNode :: Grid -> Node
goalNode g = fromJust (findNode g (gridGoalData g))

neighbours :: Grid -> Node -> [Node]
neighbours g (Node (Pos x y) _ _) =
  mapMaybe
    (findNode g . uncurry Pos)
    [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

viableNeighbours :: Grid -> Node -> [Node]
viableNeighbours g n = filter (viable n) (neighbours g n)

instance Show Grid where
  show g =
    ("\n" ++) .
    unlines . fmap row . groupBy ((==) `on` (py . nPos)) . M.elems . gridNodes $
    g
    where
      row = unwords . fmap shownode
      shownode n
        | n == goalNode g = "G"
      shownode n
        | isEmpty n = "_"
      shownode n
        | all (muchSmaller n) (neighbours g n) = "#"
      shownode n
        | nSize n < nUsed (goalNode g) = "/"
      shownode n =
        if any (viable n) (neighbours g n)
          then "+"
          else "."
      isEmpty n = nUsed n == 0
      muchSmaller n n' = nUsed n > nSize n'

-- search :: Grid -> Maybe [Grid]
-- search start =
--   fmap (head . snd) $
--   find ((0 ==) . goalDistance . head . head . snd) $
--   iterate expandSearch (S.singleton start, [[start]])
-- expandSearch :: (S.Set Grid, [[Grid]]) -> (S.Set Grid, [[Grid]])
-- expandSearch (seen, []) = (seen, [])
-- expandSearch (seen, gs@(g:_):rest) =
--   (seen', sortBy (compare `on` score) (rest ++ ((: gs) <$> new)))
--   where
--     new = filter (`S.notMember` seen) (nextsAroundGoal g)
--     seen' = seen `S.union` S.fromList new
--     score p@(g':_) = goalDistance g' + length p
--     score _ = error "empty path"
-- expandSearch (_, []:_) = error "empty path"
-- nextsAroundGoal :: Grid -> [Grid]
-- nextsAroundGoal g = wideningNexts g (goalNode g)
-- wideningNexts :: Grid -> Node -> [Grid]
-- wideningNexts g n = go (S.singleton n) n
--   where
--     nodesets = (nodesAtDistance n) <$> [1..maxDist]
--     go seen node =
--       if not (null curNexts)
--       then curNexts
--       else concatMap (wideningNexts g) unseenNeighbours
--       where
--         drop 1 $ groupBy fst $  S.elems $ gridNodes
--         unseenNeighbours = filter (`S.notMember` seen) (neighbours g node)
--         curNexts = nexts g node
-- nexts :: Grid -> Node -> [Grid]
-- nexts g from = do
--   to <- viableNeighbours g from
--   let from' =
--         from
--         { nUsed = 0
--         }
--       to' =
--         to
--         { nUsed = nUsed from
--         }
--       nodes' =
--         M.fromList [(nPos from', from'), (nPos to', to')] `M.union` gridNodes g
--   return $
--     g
--     { gridNodes = nodes'
--     , gridGoalData =
--       if nPos from == gridGoalData g
--         then nPos to'
--         else gridGoalData g
--     }
-- goalDistance :: Grid -> Int
-- goalDistance g = px (gridGoalData g) + py (gridGoalData g)
partA = length . viablePairs

main = runDay day22

day22 =
  Day
    22
    (count 2 (manyTill anyChar newline) *> many1 parseNode)
    (return . show . partA)
    (return . const "TODO")
