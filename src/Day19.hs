module Main where

import Day
import Data.Sequence (Seq)
import Data.List (find)
import qualified Data.Sequence as S

elves :: Int -> Elf
elves = (`S.index` 0) . runElves . makeElves

runElves es
  | S.length es >= 2 = runElves (S.drop 2 es S.>< S.take 1 es)
runElves es = es

data Elf = Elf
  { eNum :: !Int
  } deriving (Show)

makeElves n = Elf <$> (S.iterateN n (+ 1) 1)

partA = elves

main =
  runDay $
  Day
    19
    ((read <$> many1 digit) <* newline)
    (return . show . partA)
    (return . show . const "TODO")
