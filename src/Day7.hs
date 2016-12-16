{-# LANGUAGE TupleSections #-}

module Main where

import Day
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

main =
  runDay $
  Day
    7
    (many1 (parseIPAddress <* newline))
    (return . show . length . filter canTLS)
    (return . show . length . filter canSSL)
