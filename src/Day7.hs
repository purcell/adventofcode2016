{-# LANGUAGE TupleSections #-}

module Day7
  ( day7
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.List (partition, isInfixOf)

data Net
  = Super
  | Hyper

newtype IPAddress =
  IPAddress [(Net, String)]

abba :: String -> Bool
abba (a:b:c:d:_)
  | a /= b && a == d && b == c = True
abba (_:xs) = abba xs
abba _ = False

canTLS :: IPAddress -> Bool
canTLS (IPAddress parts) = abbas super && not (abbas hyper)
  where
    abbas = any abba . fmap snd
    (super, hyper) = partition supernet parts
    supernet (Super, _) = True
    supernet _ = False

canSSL :: IPAddress -> Bool
canSSL (IPAddress parts) =
  or
    [ [b, a, b] `isInfixOf` hyper
    | (Super, super) <- parts
    , [a, b, _] <- abas super
    , (Hyper, hyper) <- parts ]

abas :: String -> [String]
abas = go []
  where
    go acc s@(a:b:c:_)
      | a /= b && a == c = go ([a, b, c] : acc) (drop 1 s)
    go acc (_:xs) = go acc xs
    go acc _ = acc

parseIPAddress :: Parser IPAddress
parseIPAddress =
  IPAddress <$>
  many1
    (((Super, ) <$> many1 letter) <|>
     ((Hyper, ) <$> (string "[" *> many1 letter <* string "]")))

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
  print $ length $ filter canSSL input
