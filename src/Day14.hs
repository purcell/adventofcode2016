module Day14
  ( day14
  ) where

import Day
import qualified Crypto.Hash as H
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.List (group, tails)
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
  [ (n, h)
  | ((n, h, triple, _), nexts) <- zip allTriples (tail (tails allTriples))
  , any
     (hasQuintuple triple)
     (takeWhile (\(n', _, _, _) -> n' <= n + 1000) nexts) ]
  where
    allTriples :: [(Int, String, Char, String)]
    allTriples =
      [ (n, h, triple, quints)
      | (n, h) <- (id &&& hasher) <$> [0 ..]
      , let groups = bigGroups h
      , not (null groups)
      , let (triple, _) = head groups
      , let quints = fst <$> filter ((>= 5) . snd) groups ]
    hasQuintuple c (_, _, _, quints) = c `elem` quints

bigGroups
  :: Eq a
  => [a] -> [(a, Int)]
bigGroups = filter ((>= 3) . snd) . map (head &&& length) . group

partA = (!! 63) . keys . hash

partB = (!! 63) . keys . stretchHash

day14 =
  Day
    14
    (many1 letter <* newline)
    (return . show . partA)
    (return . show . partB)
