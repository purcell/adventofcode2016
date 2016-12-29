module Main where

import AdventOfCode
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

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

type Password = Seq Char

apply :: Password -> Instr -> Password
apply s (SwapPos x y) =
  S.update x (s `S.index` y) (S.update y (s `S.index` x) s)
apply s (SwapLetters a b) = replacement <$> s
  where
    replacement c
      | c == a = b
      | c == b = a
      | otherwise = c
apply s (Rotate DirLeft n) =
  uncurry (flip (<>)) (S.splitAt (n `mod` S.length s) s)
apply s (Rotate DirRight n) = apply s (Rotate DirLeft (S.length s - n))
apply s (RotatePosition c) = apply s (Rotate DirRight off)
  where
    off =
      1 + nc +
      (if nc >= 4
         then 1
         else 0)
    nc = fromMaybe (error "no such char") $ c `S.elemIndexL` s
apply s (Reverse x y)
  | y >= x =
    let (pre, rest) = S.splitAt x s
        (middle, end) = S.splitAt (y - x + 1) rest
    in pre <> S.reverse middle <> end
apply s (Move x y) = beforeY <> S.take 1 fromX <> fromY
  where
    (beforeX, fromX) = S.splitAt x s
    (beforeY, fromY) = S.splitAt y (beforeX <> S.drop 1 fromX)

unapply :: Password -> Instr -> Password
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
partA = toList . foldl apply (S.fromList "abcdefgh")

partB :: [Instr] -> String
partB = toList . foldl unapply (S.fromList "fbgdceah") . reverse

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
