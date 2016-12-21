module Main where

import Day
import Data.Sequence (Seq)
import qualified Data.Sequence as S

elves :: (Seq Elf -> Seq Elf) -> Int -> Elf
elves f n = (`S.index` 0) . f $ makeElves n

runElvesA es
  | S.length es >= 2 = runElvesA (S.drop 2 es S.>< S.take 1 es)
runElvesA es = es

runElvesB es
  | S.length es >= 2 =
    runElvesB (beforeSteal S.>< S.drop 1 fromSteal S.>< first)
  where
    (first, rest) = S.splitAt 1 es
    (beforeSteal, fromSteal) = S.splitAt (nSteal - 1) rest
    nSteal = S.length es `div` 2
runElvesB es = es

data Elf = Elf
  { eNum :: !Int
  } deriving (Show)

makeElves n = Elf <$> S.iterateN n (+ 1) 1

main =
  runDay $
  Day
    19
    ((read <$> many1 digit) <* newline)
    (return . show . elves runElvesA)
    (return . show . elves runElvesB)
