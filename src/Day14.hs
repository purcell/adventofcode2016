module Day14
  ( day14
  ) where

import Day
import qualified Crypto.Hash as H
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.List (isInfixOf, tails)
import Control.Arrow ((&&&))

hexMD5 :: String -> String
hexMD5 = show . md5 . fromString
  where
    md5 :: ByteString -> H.Digest H.MD5
    md5 = H.hash

hash :: String -> Int -> String
hash salt n = hexMD5 (salt ++ show n)

stretchHash :: String -> Int -> String
stretchHash salt n = iterate hexMD5 (salt ++ show n) !! 2017

keys :: (Int -> String) -> [(Int, String)]
keys hasher =
  let hs = (id &&& hasher) <$> [0 ..]
  in [ (n, h)
     | (n, h) <- hs
     , c <-
        take
          1
          [ x
          | x:y:z:_ <- tails h
          , x == y
          , y == z ]
     , any (replicate 5 c `isInfixOf`) (snd <$> hs) ]

partA = (!! 63) . keys . hash

partB = (!! 63) . keys . stretchHash

day14 =
  Day
    14
    (many1 letter <* newline)
    (return . show . partA)
    (return . show . partB)
