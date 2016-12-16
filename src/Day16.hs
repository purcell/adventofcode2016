module Day16
  ( day16
  ) where

import Day

expandData a = a ++ "0" ++ (dNot <$> (reverse a))
  where
    dNot '0' = '1'
    dNot '1' = '0'
    dNot _ = error "bad dNot arg"

fill len input = checksum rawData
  where
    rawData = take len minExpansion
    minExpansion = head $ dropWhile ((<= len) . length) expansions
    expansions = (iterate expandData input)

checksum xs = head $ dropWhile (even . length) $ iterate go xs
  where
    go ys@(a:b:_) =
      (if a == b
         then '1'
         else '0') :
      go (drop 2 ys)
    go _ = []

day16 =
  Day
    16
    (many (oneOf "01") <* newline)
    (return . show . fill 272)
    (return . show . fill 35651584)