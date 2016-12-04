module Day4
  -- ( day4
  -- )
where

import           Text.Parsec        hiding (State)
import           Text.Parsec.String
import Data.List (group, sort, sortBy)


data Room = Room { encName :: [String], sectorID :: Int, checksum :: String }
  deriving Show


real :: Room -> Bool
real room = checksum room == calcChecksum room

calcChecksum :: Room -> String
calcChecksum = take 5 . map snd . sortBy comparison . hist
  where
    hist = map (\s@(c:_) -> (length s, c)) . group . sort . concat . encName
    comparison (l1, c1) (l2, c2) = case compare l2 l1 of
                                 EQ -> compare c1 c2
                                 x -> x

example = Room ["aaaaa", "bbb", "z", "y", "x"] 123 "abxyz"

parseRoom :: Parser Room
parseRoom =
  Room <$> many1 (many1 letter <* string "-") <*> parseInt <*> (string "[" *> many1 letter <* string "]")
  where
    parseInt = read <$> many1 digit


loadInput :: IO [Room]
loadInput = do
  result <- parseFromFile (many1 (parseRoom <* newline) <* eof) "input/4.txt"
  case result of
    Right rooms -> return rooms
    Left e -> error (show e)

day4 :: IO ()
day4 = do
  rooms <- loadInput
  print $ sum (sectorID <$> filter real rooms)
