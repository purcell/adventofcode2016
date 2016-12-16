module Main where

import Day
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.String (fromString)

hashes :: String -> [String]
hashes seed = hexMD5 . (seed ++) . show <$> [(0 :: Integer) ..]

hexMD5 :: String -> String
hexMD5 = show . md5 . fromString
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash

interesting :: String -> Bool
interesting s = "00000" == take 5 s

interestingHashes :: String -> [String]
interestingHashes seed = filter interesting (hashes seed)

solutionA :: [String] -> String
solutionA hs = take 8 $ (!! 5) <$> hs

solutionB :: [String] -> String
solutionB hs = M.elems $ head $ dropWhile incomplete $ scanl addPair M.empty pairs
  where
    addPair m (p, c) =
      if p `M.member` m
        then m
        else M.insert p c m
    incomplete m = M.keys m /= "01234567"
    pairs = catMaybes $ (posAndChar . drop 5) <$> hs
    posAndChar (p:c:_)
      | p >= '0' && p < '8' = Just (p, c)
    posAndChar _ = Nothing

main =
  runDay $
  Day
    5
    (many1 letter <* newline)
    (return . solutionA . hashes)
    (return . solutionB . hashes)
