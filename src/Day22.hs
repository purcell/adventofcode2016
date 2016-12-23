module Main where

import Day
import Data.List (delete)

data Node = Node
  { nX :: Int
  , nY :: Int
  , nSize :: Int
  , nUsed :: Int
  } deriving (Eq)

parseNode :: Parser Node
parseNode =
  Node <$> (string "/dev/grid/node-x" *> number) <*> (string "-y" *> number) <*>
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
  , nUsed n1 > 0
  , nUsed n1 <= (nSize n2 - nUsed n2) ]

partA = length . viablePairs

main =
  runDay $
  Day
    22
    (count 2 (manyTill anyChar newline) *> many1 parseNode)
    (return . show . partA)
    (return . const "TODO")
