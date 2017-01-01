-- These are pretty much borrowed verbatim from glguy: see
-- https://github.com/glguy/advent2016/blob/master/lib/Search.hs
module Search
  ( bfs
  , bfsOn
  , astar
  , astarOn
  ) where

import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Foldable (foldl')

bfsOn
  :: Ord b
  => (a -> b) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = go Set.empty [start] []
  where
    go _ [] [] = []
    go seen [] ys = go seen (reverse ys) []
    go seen (x:xs) ys
      | rep x `Set.member` seen = go seen xs ys
      | otherwise = x : go (rep x `Set.insert` seen) xs (next x ++ ys)

bfs
  :: Ord a
  => (a -> [a]) -> a -> [a]
bfs = bfsOn id

astar
  :: Ord a
  => (a -> [(a, Int, Int)]) -> a -> [(a, Int)]
astar = astarOn id

astarOn
  :: Ord b
  => (a -> b) -> (a -> [(a, Int, Int)]) -> a -> [(a, Int)]
astarOn rep nexts start = go Set.empty (IntMap.singleton 0 [(0, start)])
  where
    go seen work =
      case pqueueNext work of
        Nothing -> []
        Just ((cost, x), work1)
          | Set.member r seen -> go seen work1
          | otherwise -> (x, cost) : go seen' work2
          where r = rep x
                seen' = Set.insert r seen
                work2 = foldl' addWork work1 (nexts x)
                addWork w (x', stepcost, heuristic) =
                  let cost' = cost + stepcost
                  in cost' `seq` pqueuePush (cost' + heuristic) (cost', x') w

pqueuePush :: Int -> a -> IntMap [a] -> IntMap [a]
pqueuePush k v = IntMap.alter aux k
  where
    aux Nothing = Just [v]
    aux (Just vs) = Just (v : vs)

pqueueNext :: IntMap [a] -> Maybe (a, IntMap [a])
pqueueNext q = do
  ((k, xs), q1) <- IntMap.minViewWithKey q
  case xs of
    [] -> error "Malformed queue"
    [x] -> Just (x, q1)
    x:rest ->
      let q2 = IntMap.insert k rest q1
      in q2 `seq` Just (x, q2)
