module Main where

import Day
import Data.List (elemIndex)

data Direction
  = DirLeft
  | DirRight

data Instr
  = SwapPos Int
            Int
  | SwapLetters Char
                Char
  | Rotate Direction
           Int
  | RotatePosition Char
  | Reverse Int
            Int
  | Move Int
         Int

apply :: String -> Instr -> String
apply s (SwapPos x y) = zipWith swap [0 ..] s
  where
    swap n c
      | n == x = s !! y
      | n == y = s !! x
      | otherwise = c
apply s (SwapLetters a b) = map replacement s
  where
    replacement c
      | c == a = b
      | c == b = a
      | otherwise = c
apply s (Rotate DirLeft n) = take (length s) $ drop (n `mod` length s) (cycle s)
apply s (Rotate DirRight n) =
  take (length s) $ drop (length s - (n `mod` length s)) (cycle s)
apply s (RotatePosition c) =
  apply
    s
    (Rotate
       DirRight
       (1 + nc +
        (if nc >= 4
           then 1
           else 0)))
  where
    nc =
      case c `elemIndex` s of
        Just x -> x
        _ -> error "no such char"
apply s (Reverse x y)
  | y >= x =
    let (pre, rest) = splitAt x s
        (middle, end) = splitAt (y - x + 1) rest
    in pre ++ reverse middle ++ end
apply s (Move x y) =
  let withoutx =
        [ c
        | (n, c) <- zip [0 ..] s
        , n /= x ]
      (before, after) = splitAt y withoutx
  in before ++ [s !! x] ++ after

unapply :: String -> Instr -> String
unapply s (Rotate DirRight n) = apply s $ Rotate DirLeft n
unapply s (Rotate DirLeft n) = apply s $ Rotate DirRight n
unapply s (SwapPos a b) = apply s $ SwapPos b a
unapply s (SwapLetters a b) = apply s $ SwapLetters b a
unapply s r@(RotatePosition _) =
  head
    [ s'
    | n <- [0 .. (length s)]
    , let s' = apply s (Rotate DirLeft n)
    , apply s' r == s ]
unapply s r@(Reverse _ _) = apply s r
unapply s (Move a b) = apply s $ Move b a

partA :: [Instr] -> String
partA = foldl apply "abcdefgh"

partB :: [Instr] -> String
partB = foldl unapply "fbgdceah" . reverse

parseInstr :: Parser Instr
parseInstr =
  try
    (SwapPos <$> (string "swap position " *> num) <*>
     (string " with position " *> num)) <|>
  try
    (SwapLetters <$> (string "swap letter " *> anyChar) <*>
     (string " with letter " *> anyChar)) <|>
  try
    (Rotate <$> (string "rotate " *> direction) <*>
     (string " " *> num <* string " step" <* optional (char 's'))) <|>
  try
    (RotatePosition <$>
     (string "rotate based on position of letter " *> anyChar)) <|>
  try
    (Reverse <$> (string "reverse positions " *> num) <*>
     (string " through " *> num)) <|>
  try
    (Move <$> (string "move position " *> num) <*>
     (string " to position " *> num))
  where
    num = read <$> many1 digit
    direction =
      (string "left" *> pure DirLeft) <|> (string "right" *> pure DirRight)

main =
  runDay $
  Day 21 (many1 (parseInstr <* newline)) (return . partA) (return . partB)
