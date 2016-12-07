{-# LANGUAGE TupleSections #-}

module Day7
  ( day7
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String

data Part
  = Plain
  | Bracketed

data IPAddress =
  IPAddress [(Part, String)]

abba :: String -> Bool
abba (a:b:c:d:_)
  | a /= b && a == d && b == c = True
abba (x:xs) = abba xs
abba _ = False

canTLS :: IPAddress -> Bool
canTLS (IPAddress parts) =
  any abba (snd <$> filter plain parts) &&
  not (any abba (snd <$> filter bracketed parts))
  where
    plain (Plain, _) = True
    plain _ = False
    bracketed (Bracketed, _) = True
    bracketed _ = False

parseIPAddress :: Parser IPAddress
parseIPAddress =
  IPAddress <$>
  many1
    (((Plain, ) <$> many1 letter) <|>
     ((Bracketed, ) <$> (string "[" *> many1 letter <* string "]")))

loadInput :: IO [IPAddress]
loadInput = do
  result <-
    parseFromFile (many1 (parseIPAddress <* newline) <* eof) "input/7.txt"
  case result of
    Right xs -> return xs
    Left e -> error (show e)

day7 :: IO ()
day7 = do
  input <- loadInput
  print $ length $ filter canTLS input
