{-# LANGUAGE TupleSections #-}

module Day7
  ( day7
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.List (partition, intersect)

data Net
  = Super
  | Hyper

newtype IPAddress =
  IPAddress [(Net, String)]

superHyper :: IPAddress -> ([String], [String])
superHyper (IPAddress parts) = (snd <$> super, snd <$> hyper)
  where
    (super, hyper) = partition supernet parts
    supernet (Super, _) = True
    supernet _ = False

abba :: String -> Bool
abba (a:b:c:d:_)
  | a /= b && a == d && b == c = True
abba (_:xs) = abba xs
abba _ = False

canTLS :: IPAddress -> Bool
canTLS a = any abba super && not (any abba hyper)
  where
    (super, hyper) = superHyper a

canSSL :: IPAddress -> Bool
canSSL a =
  not . null $ (invertABA <$> concatMap abas super) `intersect` concatMap abas hyper
  where
    (super, hyper) = superHyper a

invertABA :: String -> String
invertABA (a:b:_) = [b, a, b]
invertABA _ = undefined

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
