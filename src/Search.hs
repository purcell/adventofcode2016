module Search where

import qualified Data.Set as S

bfsOn
  :: Ord b
  => (a -> b) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = go S.empty [start] []
  where
    go _ [] [] = []
    go seen [] ys = go seen (reverse ys) []
    go seen (x:xs) ys
      | rep x `S.member` seen = go seen xs ys
      | otherwise = x : go (rep x `S.insert` seen) xs (next x ++ ys)

bfs
  :: Ord a
  => (a -> [a]) -> a -> [a]
bfs = bfsOn id
