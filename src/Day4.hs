module Main where

import AdventOfCode
import Data.List (group, sort, sortBy)

data Room = Room
  { encName :: [String]
  , sectorID :: Int
  , checksum :: String
  } deriving (Show)

real :: Room -> Bool
real room = checksum room == calcChecksum room

decrypt :: Room -> String
decrypt room = unwords $ decryptWord <$> encName room
  where
    decryptWord = map rotateLetter
    rotateLetter l = ([l .. 'z'] ++ ['a' ..]) !! (sectorID room `mod` 26)

calcChecksum :: Room -> String
calcChecksum = take 5 . map snd . sortBy comparison . hist
  where
    hist = map (\s@(c:_) -> (length s, c)) . group . sort . concat . encName
    comparison (l1, c1) (l2, c2) =
      case compare l2 l1 of
        EQ -> compare c1 c2
        x -> x

example = Room ["aaaaa", "bbb", "z", "y", "x"] 123 "abxyz"

example2 = Room ["qzmt", "zixmtkozy", "ivhz"] 343 "abxyz"

parseRoom :: Parser Room
parseRoom =
  Room <$> many1 (many1 letter <* string "-") <*> parseInt <*>
  (string "[" *> many1 letter <* string "]")
  where
    parseInt = read <$> many1 digit

main =
  runDay $
  Day
    4
    (many1 (parseRoom <* newline))
    (return . show . sum . fmap sectorID . filter real)
    (return .
     show . filter (("northpole object storage" ==) . decrypt) . filter real)
