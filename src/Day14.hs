module Day14
  ( day14
  ) where

import Day
import qualified Crypto.Hash as H
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.List (group)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Arrow ((&&&))

hash :: String -> Int -> String
hash salt n = hexMD5 (salt ++ show n)

keys :: String -> [(Int, String)]
keys = go . hashes
  where
    go ((Hash n hs c _):rest) =
      if any (hasQuintuple c) (takeWhile ((<= (n + 1000)) . hIndex) rest)
        then (n, hs) : go rest
        else go rest

hashes salt = mapMaybe (makeHashIfTriple salt) [0 ..]

data Hash = Hash
  { hIndex :: !Int
  , hHash :: !String
  , hFirstTriple :: !Char
  , hQuintuples :: !String
  } deriving (Show)

makeHashIfTriple :: String -> Int -> Maybe Hash
makeHashIfTriple salt n = do
  triple <- firstTriple
  return $ Hash n h triple quintuples
  where
    h = hash salt n
    groups = bigGroups h
    firstTriple = fst <$> listToMaybe groups
    quintuples = fst <$> filter ((>= 5) . snd) groups

hasQuintuple c h = c `elem` hQuintuples h

bigGroups
  :: Eq a
  => [a] -> [(a, Int)]
bigGroups = filter ((>= 3) . snd) . map (head &&& length) . group

hexMD5 :: String -> String
hexMD5 = show . md5 . fromString
  where
    md5 :: ByteString -> H.Digest H.MD5
    md5 = H.hash

partA = (!! 63) . keys

day14 =
  Day
    14
    (many1 letter <* newline)
    (return . show . partA)
    (return . show . const "TODO")
