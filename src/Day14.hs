module Main where

import AdventOfCode
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

hash :: Int -> String -> Int -> String
hash stretches salt n = iterate hexMD5 (salt ++ show n) !! stretches

keys :: (Int -> String) -> [(Int, String)]
keys hasher = go ((id &&& hasher) <$> [0 ..])
  where
    go (h@(n, s):hs) =
      [ h
      | c <-
         take
           1
           [ x
           | x:y:z:_ <- tails s
           , x == y
           , y == z ]
      , any (replicate 5 c `isInfixOf`) (snd <$> take 1000 hs) ] ++
      go hs

solve stretches = (!! 63) . keys . hash stretches

main =
  runDay $
  Day
    14
    (many1 letter <* newline)
    (return . show . solve 1)
    (return . show . solve 2017)
