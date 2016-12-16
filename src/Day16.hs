module Main where

import Day

expandData :: String -> String
expandData a = a ++ "0" ++ (dNot <$> reverse a)
  where
    dNot '0' = '1'
    dNot '1' = '0'
    dNot _ = error "bad dNot arg"

fill :: Int -> String -> String
fill len input = checksum rawData
  where
    rawData = take len minExpansion
    minExpansion = head $ dropWhile ((<= len) . length) expansions
    expansions = iterate expandData input

checksum :: String -> String
checksum xs = head $ dropWhile (even . length) $ iterate go xs
  where
    go ys@(a:b:_) =
      (if a == b
         then '1'
         else '0') :
      go (drop 2 ys)
    go _ = []

main =
  runDay $
  Day
    16
    (many (oneOf "01") <* newline)
    (return . fill 272)
    (return . fill 35651584)
