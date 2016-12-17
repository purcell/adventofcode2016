module Main where

import Day
import qualified Crypto.Hash as H
import Data.ByteString (ByteString)
import Data.String (fromString)

hexMD5 :: String -> String
hexMD5 = show . md5 . fromString
  where
    md5 :: ByteString -> H.Digest H.MD5
    md5 = H.hash

type Pos = (Int, Int)

paths :: String -> (String, Pos) -> [(String, Pos)]
paths seed start = go [start]
  where
    go :: [(String, Pos)] -> [(String, Pos)]
    go [] = []
    go (x@(path, pos):xs) = [x] ++ go xs ++ go nexts
      where
        hash = hexMD5 (seed ++ path)
        nexts =
          [ (path ++ [dir], pos')
          | (dir, c) <- zip "UDLR" hash
          , c `elem` "bcdef"
          , let pos'@(x', y') = move pos dir
          , x' >= 0
          , x' <= 3
          , y' >= 0
          , y' <= 3 ]

move (x, y) 'U' = (x, y - 1)
move (x, y) 'D' = (x, y + 1)
move (x, y) 'L' = (x - 1, y)
move (x, y) 'R' = (x + 1, y)
move _ _ = error "bad move"

partA :: String -> String
partA = fst . head . dropWhile (((3, 3) /=) . snd) . flip paths ("", (0, 0))

main =
  runDay $
  Day 17 (many letter <* newline) (return . partA) (return . const "TODO")
