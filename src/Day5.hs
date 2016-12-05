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

solutionA :: [String] -> String
solutionA hs = take 8 $ (!! 5) <$> hs

solutionB :: [String] -> String
solutionB hs =
  M.elems $ head $ dropWhile incomplete $ scanl addPair M.empty pairs
  where
    addPair m (p, c) = if p `M.member` m then m else M.insert p c m
    incomplete m = M.keys m /= "01234567"
    pairs = catMaybes $ (posAndChar . drop 5) <$> hs
    posAndChar (p:c:_) | p >= '0' && p < '8' = Just (p, c)
    posAndChar _ = Nothing


day5 :: IO ()
day5 = do
  let hs = interestingHashes "ojvtpuvg"
  putStrLn $ solutionA hs
  putStrLn $ solutionB hs
