module Day5
  ( day5
  )

where

import Data.Digest.Pure.MD5 as MD5
import Data.String
import Data.Maybe (catMaybes)
import qualified Data.Map as M


hashes :: String -> [String]
hashes seed = hexMD5 . (seed ++) . show <$> [(0::Integer)..]

hexMD5 :: String -> String
hexMD5 = show . md5 . fromString

interesting :: String -> Bool
interesting s = "00000" == take 5 s

interestingHashes :: String -> [String]
interestingHashes seed = filter interesting (hashes seed)

solutionA :: String -> String
solutionA seed = take 8 $ (!! 5) <$> interestingHashes seed

solutionB :: String -> String
solutionB seed =
  M.elems $ head $ dropWhile incomplete $ scanl addPair M.empty pairs
  where
    addPair m (p, c) = if p `M.member` m then m else M.insert p c m
    incomplete m = M.keys m /= "01234567"
    pairs = catMaybes $ (posAndChar . drop 5) <$> interestingHashes seed
    posAndChar (p:c:_) | p >= '0' && p < '8' = Just (p, c)
    posAndChar _ = Nothing


day5 :: IO ()
day5 = do
  putStrLn $ solutionA "ojvtpuvg"
  putStrLn $ solutionB "ojvtpuvg"
